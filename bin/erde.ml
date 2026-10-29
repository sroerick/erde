(* erde -- the gateway's command line.

   Commands that spend money: turn.  Commands that do not: providers, plan,
   journal, get.  [plan] is the dry run that shows what a microturn would
   send, what it would cost, and which provider would serve it. *)

open Cmdliner
open Erde

let framework_policy =
  "Erde microturn. A turn is one model call with a fixed budget. Tool calls \
   are typed values, not shell text. State that must outlive the call belongs \
   in a claim, not in this prompt."

let config_term =
  Arg.(
    value
    & opt string "erde.json"
    & info [ "config"; "c" ] ~docv:"PATH" ~doc:"Instance config (providers, routes, journal).")

(* ---------- helpers ---------- *)

let build_segments ~claims_file ~project_file ~task =
  let read_opt = function
    | None -> ""
    | Some f -> ( match Config.read_file f with Ok s -> s | Error m -> failwith m)
  in
  let claims =
    match claims_file with
    | None -> []
    | Some f -> (
        match Config.read_file f with
        | Error m -> failwith m
        | Ok s -> (
            match Yojson.Safe.from_string s with
            | exception Yojson.Json_error m -> failwith m
            | `List l ->
                List.filter_map
                  (function
                    | `Assoc kvs ->
                        let g k = match List.assoc_opt k kvs with Some (`String s) -> s | _ -> "" in
                        Some { Segments.Claim_set.path = g "path"; value = g "value"; source = g "source" }
                    | _ -> None)
                  l
            | _ -> failwith "claims file must be a JSON array"))
  in
  let segs =
    [
      { Segments.kind = Segments.Framework; id = "policy-v0"; text = framework_policy };
      { Segments.kind = Segments.Tools; id = "tools-v0"; text = "no tool hosts wired in this build" };
      { Segments.kind = Segments.Claims; id = "none"; text = "" };
      { Segments.kind = Segments.Project; id = "project"; text = read_opt project_file };
      { Segments.kind = Segments.Task; id = "task"; text = task };
    ]
  in
  let segs =
    List.map
      (fun s ->
        if s.Segments.kind = Segments.Claims && claims <> [] then Segments.Claim_set.segment claims
        else s)
      segs
  in
  match Segments.make segs with Ok t -> t | Error m -> failwith m

let print_segments segs =
  Printf.printf "segments (%d, prefix %d tokens, hash %s)\n" (List.length segs)
    (Segments.tokens segs) (String.sub (Segments.prefix_hash segs) 0 16);
  List.iter
    (fun (i, kind, hash, toks) ->
      Printf.printf "  [%d] %-9s %6d tok  %s\n" i kind toks (String.sub hash 0 16))
    (Segments.boundaries segs)

(* ---------- commands ---------- *)

let providers config =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg ->
      print_endline "providers (no secrets shown)";
      List.iter
        (fun (p : Config.provider) ->
          let cache_mode =
            match p.Config.cache.Config.mode with
            | Config.Explicit -> "explicit"
            | Config.Implicit -> "implicit"
          in
          Printf.printf "  %-10s %-9s %-30s key=%-20s live=%-5b cache=%-9s ttl_s=%-6d min_prefix=%d\n"
            p.Config.name
            (match p.Config.wire with Config.Anthropic -> "anthropic" | Config.Openai -> "openai")
            p.Config.base_url p.Config.key_env p.Config.live cache_mode
            p.Config.cache.Config.ttl_s p.Config.cache.Config.min_prefix_tokens)
        cfg.Config.providers;
      print_endline "routes";
      List.iter
        (fun (r : Config.route) ->
          let cands =
            List.map
              (fun n ->
                match Config.provider cfg n with
                | Some p when p.Config.live -> n
                | Some _ -> n ^ "(retired)"
                | None -> n ^ "(unknown)")
              r.Config.candidates
          in
          Printf.printf "  %-24s %s\n" r.Config.model (String.concat " > " cands))
        cfg.Config.routes;
      0

let plan config model task claims project =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      let segs = build_segments ~claims_file:claims ~project_file:project ~task in
      print_segments segs;
      match Config.choose cfg ~model () with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok (p, route) ->
          let plan = Cache_strategy.plan p segs in
          Printf.printf "route      %s -> %s\n" model p.Config.name;
          Printf.printf "reason     %s\n" route;
          Printf.printf "cache      %s, breakpoints [%s], ttl %ds\n"
            (match plan.Cache_strategy.mode with
            | Config.Explicit -> "explicit"
            | Config.Implicit -> "implicit")
            (String.concat "," (List.map string_of_int plan.Cache_strategy.breakpoints))
            plan.Cache_strategy.ttl_s;
          Printf.printf "cache why  %s\n" plan.Cache_strategy.reason;
          Printf.printf "head       %s\n" plan.Cache_strategy.head;
          let est_in = plan.Cache_strategy.prefix_tokens in
          let est_cost =
            Wire.cost_usd p { Wire.input = est_in; output = 0; cache_write = 0; cache_read = 0 }
          in
          Printf.printf "estimate   %d input tokens, cold call ~$%.4f in, warm ~$%.4f\n" est_in
            est_cost
            (Wire.cost_usd p
               { Wire.input = 0; output = 0; cache_write = 0; cache_read = est_in });
          0)

let turn config model task max_tokens warm =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      let segs = build_segments ~claims_file:None ~project_file:None ~task in
      print_segments segs;
      let model_opt = if model = "" then None else Some model in
      let model =
        match model_opt with
        | Some m -> m
        | None -> (
            match cfg.Config.routes with
            | r :: _ -> r.Config.model
            | [] ->
                prerr_endline "erde: no model given and no routes configured";
                exit 1)
      in
      let warm = if warm = "" then None else Some warm in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        Turn.run ~env ~sw cfg ~model ~system:(Segments.canonical segs) segs ~task ~max_tokens
          ?warm ()
      with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok o ->
          Printf.printf "\nprovider   %s (%s)\n" o.Turn.provider o.Turn.route;
          Printf.printf "latency    %d ms\n" o.Turn.latency_ms;
          (match o.Turn.response.Wire.text with
          | Some t -> Printf.printf "text       %s\n" t
          | None -> ());
          List.iter
            (fun (tc : Wire.tool_call) ->
              Printf.printf "tool_call  %s %s\n" tc.Wire.name
                (Yojson.Safe.to_string tc.Wire.args))
            o.Turn.response.Wire.tool_calls;
          Printf.printf "usage      in=%d out=%d cache_write=%d cache_read=%d\n"
            o.Turn.response.Wire.usage.Wire.input
            o.Turn.response.Wire.usage.Wire.output
            o.Turn.response.Wire.usage.Wire.cache_write
            o.Turn.response.Wire.usage.Wire.cache_read;
          Printf.printf "cost       $%.6f\n" o.Turn.cost_usd;
          0)

let journal config n =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      match Journal.read cfg.Config.journal with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok rows ->
          let total = List.length rows in
          let shown = List.filteri (fun i _ -> i >= total - n) rows in
          List.iter
            (fun (r : Journal.row) ->
              Printf.printf
                "%4d %-7s %-10s %-22s in=%-6d cached=%-6d write=%-6d out=%-5d %6dms $%.6f %s\n"
                r.Journal.seq (Journal.kind_name r.Journal.kind) r.Journal.provider
                r.Journal.model r.Journal.usage.Wire.input r.Journal.usage.Wire.cache_read
                r.Journal.usage.Wire.cache_write r.Journal.usage.Wire.output
                r.Journal.latency_ms r.Journal.cost_usd r.Journal.outcome)
            shown;
          Printf.printf "%d rows\n" total;
          List.iter print_endline (Journal.hit_rates rows);
          0)

let get url =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Http_client.request ~env ~sw ~meth:"GET" ~url ~headers:[ ("accept", "application/json") ] () with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok (status, body) ->
      Printf.printf "http %d, %d bytes\n%s\n" status (String.length body)
        (String.sub body 0 (min 600 (String.length body)));
      if status >= 200 && status < 300 then 0 else 2

let model_term =
  Arg.(value & opt string "" & info [ "model"; "m" ] ~docv:"MODEL" ~doc:"Model id (defaults to the first route).")

let task_term =
  Arg.(value & opt string "" & info [ "task"; "t" ] ~docv:"TEXT" ~doc:"The microturn's task text.")

let max_tokens_term =
  Arg.(value & opt int 1024 & info [ "max-tokens" ] ~docv:"N" ~doc:"Output token ceiling.")

let warm_term =
  Arg.(value & opt string "" & info [ "warm" ] ~docv:"PROVIDER" ~doc:"Provider holding the warm prefix.")

let claims_term =
  Arg.(value & opt (some string) None & info [ "claims" ] ~docv:"FILE" ~doc:"Claims as a JSON array of {path,value,source}.")

let project_term =
  Arg.(value & opt (some string) None & info [ "project" ] ~docv:"FILE" ~doc:"Project context segment.")

let url_term = Arg.(required & pos 0 (some string) None & info [] ~docv:"URL")

let cmd_providers =
  Cmd.v (Cmd.info "providers" ~doc:"List providers and routes (never prints secrets).")
    Term.(const providers $ config_term)

let cmd_plan =
  Cmd.v (Cmd.info "plan" ~doc:"Dry run: segments, prefix hash, cache plan, routing. Spends nothing.")
    Term.(const plan $ config_term $ model_term $ task_term $ claims_term $ project_term)

let cmd_turn =
  Cmd.v (Cmd.info "turn" ~doc:"Run one microturn and journal it.")
    Term.(const turn $ config_term $ model_term $ task_term $ max_tokens_term $ warm_term)

let n_term = Arg.(value & opt int 10 & info [ "n" ] ~docv:"N" ~doc:"How many rows to show.")

let cmd_journal =
  Cmd.v (Cmd.info "journal" ~doc:"Tail the journal and print the cache hit rates.")
    Term.(const journal $ config_term $ n_term)

let cmd_get =
  Cmd.v (Cmd.info "get" ~doc:"Fetch a URL and print the status (the TLS/HTTP smoke test).")
    Term.(const get $ url_term)

let () =
  exit
    (Cmd.eval'
       (Cmd.group (Cmd.info "erde" ~version:"0.1"
                     ~doc:"Erde Lang: a substrate around tuna. One microturn, journaled, cache-aware.")
          [ cmd_providers; cmd_plan; cmd_turn; cmd_journal; cmd_get ]))
