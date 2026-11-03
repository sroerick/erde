(* erde -- the gateway's command line.

   Commands that spend money: turn.  Everything else is dry: providers and
   plan never leave the box, claims and tool talk to tuna, journal and
   verdict read and correct the record. *)

open Cmdliner
open Erde

let substr s n = if String.length s <= n then s else String.sub s 0 n

let config_term =
  Arg.(
    value
    & opt string "erde.json"
    & info [ "config"; "c" ] ~docv:"PATH" ~doc:"Instance config (providers, routes, authority, grants).")

let model_term =
  Arg.(value & opt string "" & info [ "model"; "m" ] ~docv:"MODEL" ~doc:"Model id (defaults to the first route).")

let task_term =
  Arg.(value & opt string "" & info [ "task"; "t" ] ~docv:"TEXT" ~doc:"The microturn's task text.")

let max_tokens_term = Arg.(value & opt int 1024 & info [ "max-tokens" ] ~docv:"N" ~doc:"Output token ceiling.")
let max_steps_term = Arg.(value & opt int 3 & info [ "max-steps" ] ~docv:"N" ~doc:"Model calls allowed in this microturn.")

let stream_term =
  Arg.(value & flag & info [ "stream" ] ~doc:"Ask the provider to stream the response and assemble it.")

let warm_term =
  Arg.(value & opt string "" & info [ "warm" ] ~docv:"PROVIDER" ~doc:"Provider holding the warm prefix.")

let claims_term =
  Arg.(value & opt (some string) None & info [ "claims" ] ~docv:"FILE" ~doc:"Claims as a JSON array of {path,value,source} (offline).")

let prefix_term =
  Arg.(value & opt string "" & info [ "prefix" ] ~docv:"P" ~doc:"Tree prefix (defaults to the configured claims prefix).")

let project_term = Arg.(value & opt (some string) None & info [ "project" ] ~docv:"FILE" ~doc:"Project context segment.")
let n_term = Arg.(value & opt int 10 & info [ "n" ] ~docv:"N" ~doc:"How many rows to show.")
let claims_limit_term =
  Arg.(
    value
    & opt int 12
    & info [ "claims-limit" ] ~docv:"N"
        ~doc:"How many claims to fetch into the turn prefix (each value is one tree.get).")

let limit_term =
  Arg.(value & opt int 25 & info [ "limit" ] ~docv:"N" ~doc:"How many claim values to fetch (the list is one call, each value is one get).")
let url_term = Arg.(required & pos 0 (some string) None & info [] ~docv:"URL")
let prim_term = Arg.(required & pos 0 (some string) None & info [] ~docv:"PRIM" ~doc:"Tool prim, e.g. tree.get")
let path_term = Arg.(value & pos 1 (some string) None & info [] ~docv:"PATH")
let ref_term = Arg.(required & opt (some int) None & info [ "ref" ] ~docv:"SEQ" ~doc:"The row this verdict corrects.")
let status_term = Arg.(value & opt string "refuted" & info [ "status" ] ~docv:"S" ~doc:"refuted | stale | superseded | upheld")
let note_term = Arg.(value & opt string "" & info [ "note" ] ~docv:"TEXT" ~doc:"Why.")

let claims_of_file = function
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
                      let g k = match List.assoc_opt k kvs with Some (`String v) -> v | _ -> "" in
                      Some { Segments.Claim_set.path = g "path"; value = g "value"; source = g "source" }
                  | _ -> None)
                l
          | _ -> failwith "claims file must be a JSON array"))

let claims_mode_term =
  Arg.(
    value
    & opt string "cited"
    & info [ "claims-mode" ]
        ~docv:"MODE"
        ~doc:"full | cited | truncN (cited keeps the prefix small: the value stays a tree.get away)")

let segments_of ~claims ~claims_mode ~project_file ~task =
  let project =
    match project_file with
    | None -> ""
    | Some f -> ( match Config.read_file f with Ok s -> s | Error m -> failwith m)
  in
  let mode =
    match Prompt.parse_claims_mode claims_mode with Ok m -> m | Error m -> failwith m
  in
  match Prompt.default ~project ~claims ~claims_mode:mode ~task () with
  | Ok t -> t
  | Error m -> failwith m

let print_segments segs =
  Printf.printf "segments (%d, prefix %d tokens, hash %s)\n" (List.length segs)
    (Segments.tokens segs) (substr (Segments.prefix_hash segs) 16);
  List.iter
    (fun (i, kind, hash, toks) -> Printf.printf "  [%d] %-9s %6d tok  %s\n" i kind toks (substr hash 16))
    (Segments.boundaries segs)

let authority_of cfg =
  match Config.authority_token cfg with
  | Ok token -> Ok (Authority.make ~base_url:cfg.Config.authority.Config.base_url ~token)
  | Error _ as e -> e

let first_route_model cfg =
  match cfg.Config.routes with
  | r :: _ -> Some r.Config.model
  | [] -> None

let with_authority cfg f =
  match Config.load cfg with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      match authority_of cfg with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok auth -> f cfg auth)

(* ---------- commands ---------- *)

let providers config =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg ->
      print_endline "providers (no secrets shown)";
      List.iter
        (fun (p : Config.provider) ->
          let cache_mode =
            match p.Config.cache.Config.mode with Config.Explicit -> "explicit" | Config.Implicit -> "implicit"
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
      Printf.printf "authority  %s key=%s claims_prefix=%s\n" cfg.Config.authority.Config.base_url
        cfg.Config.authority.Config.token_env cfg.Config.authority.Config.claims_prefix;
      print_endline "grants";
      List.iter
        (fun (g : Grants.row) ->
          Printf.printf "  %-10s %-10s %s%s\n" g.Grants.id g.Grants.prim
            (match g.Grants.path_prefix with Some p -> p | None -> "*")
            (if g.Grants.live then "" else " (dead)"))
        cfg.Config.grants;
      0

let plan config model task claims claims_mode project =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      let segs = segments_of ~claims:(claims_of_file claims) ~claims_mode ~project_file:project ~task in
      print_segments segs;
      let model = if model <> "" then model else match first_route_model cfg with Some m -> m | None -> "" in
      match Config.choose cfg ~model () with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok (p, route) ->
          let plan = Cache_strategy.plan p segs in
          Printf.printf "route      %s -> %s\n" model p.Config.name;
          Printf.printf "reason     %s\n" route;
          Printf.printf "cache      %s, breakpoints [%s], ttl %ds\n"
            (match plan.Cache_strategy.mode with Config.Explicit -> "explicit" | Config.Implicit -> "implicit")
            (String.concat "," (List.map string_of_int plan.Cache_strategy.breakpoints))
            plan.Cache_strategy.ttl_s;
          Printf.printf "cache why  %s\n" plan.Cache_strategy.reason;
          Printf.printf "head       %s\n" plan.Cache_strategy.head;
          let est_in = plan.Cache_strategy.prefix_tokens in
          Printf.printf "estimate   %d input tokens, cold ~$%.4f, warm ~$%.4f\n" est_in
            (Wire.cost_usd p { Wire.input = est_in; output = 0; cache_write = 0; cache_read = 0 })
            (Wire.cost_usd p { Wire.input = 0; output = 0; cache_write = 0; cache_read = est_in });
          0)

(* Claims arrive with raw ternary values; decode chunked-claim text at
   ingest so the prompt segment carries readable content. *)
let decode_claim (c : Segments.Claim_set.claim) : Segments.Claim_set.claim =
  match Treecalc.from_ternary c.Segments.Claim_set.value with
  | Ok v -> (match Treecalc.as_chunked_text v with Some s -> { c with Segments.Claim_set.value = s } | None -> c)
  | Error _ -> c

let decode_claims cs = List.map decode_claim cs

let claims config prefix limit claims_mode =
  with_authority config (fun cfg auth ->
      let prefix = if prefix <> "" then prefix else cfg.Config.authority.Config.claims_prefix in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (match Authority.whoami ~env ~sw auth with
      | Ok (name, id) -> Printf.printf "whoami     %s (%s)\n" name id
      | Error m -> Printf.printf "whoami     failed: %s\n" m);
      match Authority.claims_capped ~env ~sw auth ~prefix ~cap:limit with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok (cs, total) ->
          Printf.printf "claims     %d of %d under %s (values fetched for %d)\n" (List.length cs) total
            prefix (List.length cs);
          List.iter
            (fun (c : Segments.Claim_set.claim) ->
              Printf.printf "  %-40s %-24s [%s]\n" c.Segments.Claim_set.path
                (substr c.Segments.Claim_set.value 22)
                (substr c.Segments.Claim_set.source 12))
            cs;
          let segs = segments_of ~claims:(decode_claims cs) ~claims_mode ~project_file:None ~task:"" in
          print_segments segs;
          let full = segments_of ~claims:(decode_claims cs) ~claims_mode:"full" ~project_file:None ~task:"" in
          let (_, _, _, full_tokens) =
            List.nth (Segments.boundaries full) (List.length (Segments.boundaries full) - 1)
          in
          Printf.printf "density    claims=%s renders this prefix at %d tokens; full would be %d (%dx)\n"
            claims_mode (Segments.tokens segs) full_tokens
            (if Segments.tokens segs = 0 then 0 else full_tokens / Segments.tokens segs);
          0)

let arg_term =
  Arg.(
    value & opt_all string []
    & info [ "arg" ] ~docv:"K=V"
      ~doc:"Tool argument, repeatable (dry `tool` calls only, e.g. --arg content=hi).")

let tool config prim path extra =
  with_authority config (fun cfg auth ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let args =
        let base =
          match path with
          | None -> []
          | Some p -> [ ((if prim = "tree.list" then "prefix" else "path"), `String p) ]
        in
        let extra =
          List.filter_map
            (fun kv ->
              match String.index_opt kv '=' with
              | None -> None
              | Some i ->
                  Some
                    ( String.sub kv 0 i,
                      `String (String.sub kv (i + 1) (String.length kv - i - 1)) ))
            extra
        in
        `Assoc (base @ extra)
      in
      match Tools.dispatch ~env ~sw ~authority:auth ~grants:cfg.Config.grants { Tools.prim; args } with
      | Ok j ->
          Printf.printf "%s\n" (Yojson.Safe.pretty_to_string j);
          0
      | Error m ->
          Printf.printf "refused: %s\n" m;
          1)

(* The operator's write hand for tests and standing-task edits: same grant
   gate and chunked-claim encoding the model's tree.put goes through. *)
let put config path text =
  with_authority config (fun cfg auth ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        Tools.dispatch ~env ~sw ~authority:auth ~grants:cfg.Config.grants
          {
            Tools.prim = "tree.put";
            args = `Assoc [ ("path", `String path); ("value_text", `String text) ]
          }
      with
      | Ok j ->
          Printf.printf "%s\n" (Yojson.Safe.pretty_to_string j);
          0
      | Error m ->
          Printf.printf "refused: %s\n" m;
          1)

let turn config model task max_tokens max_steps warm stream claims_mode claims_limit =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      match authority_of cfg with
      | Error m ->
          prerr_endline ("erde: " ^ m);
          1
      | Ok auth ->
          let claims =
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            match
              Authority.claims_capped ~env ~sw auth
                ~prefix:cfg.Config.authority.Config.claims_prefix
                ~cap:claims_limit
            with
            | Ok (cs, total) ->
                Printf.printf "claims     %d of %d from %s\n" (List.length cs) total
                  cfg.Config.authority.Config.claims_prefix;
                decode_claims cs
            | Error m ->
                prerr_endline
                  (Printf.sprintf "erde: claims fetch failed (%s); the identity is not optional, refusing to \
                                   run anonymous" m);
                exit 1
          in
          let segs = segments_of ~claims ~claims_mode ~project_file:None ~task in
          print_segments segs;
          let model =
            if model <> "" then model
            else match first_route_model cfg with Some m -> m | None -> ""
          in
          let warm = if warm = "" then None else Some warm in
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          match
            Turn.run ~env ~sw ~cfg ~authority:auth ~grants:cfg.Config.grants ~model
              ~system:(Segments.canonical segs) segs ~task ~max_tokens ~max_steps ~stream ?warm ()
          with
          | Error m -> prerr_endline ("erde: " ^ m); 1
          | Ok r ->
              List.iter
                (fun (s : Turn.step) ->
                  Printf.printf "step %d     %s %s -> %s\n" s.Turn.ordinal s.Turn.prim
                    (Yojson.Safe.to_string s.Turn.args)
                    (if s.Turn.allowed then substr s.Turn.result 120
                     else "REFUSED " ^ substr s.Turn.result 120))
                r.Turn.steps;
              (match r.Turn.outcome with
              | None -> Printf.printf "budget     step budget exhausted after %d calls\n" r.Turn.calls
              | Some o ->
                  Printf.printf "provider   %s (%s)\n" o.Turn.provider o.Turn.route;
                  Printf.printf "latency    %d ms\n" o.Turn.latency_ms;
                  (match o.Turn.response.Wire.text with
                  | Some t -> Printf.printf "text       %s\n" t
                  | None -> ());
                  Printf.printf "usage      in=%d out=%d cache_write=%d cache_read=%d\n"
                    o.Turn.response.Wire.usage.Wire.input o.Turn.response.Wire.usage.Wire.output
                    o.Turn.response.Wire.usage.Wire.cache_write
                    o.Turn.response.Wire.usage.Wire.cache_read;
                  Printf.printf "cost       $%.6f\n" o.Turn.cost_usd);
              Printf.printf "steps      %d tool calls, %d model calls\n" (List.length r.Turn.steps)
                r.Turn.calls;
              0)

let journal config n =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg -> (
      match Journal.read cfg.Config.journal with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok rows ->
          let total = List.length rows in
          List.iter
            (fun (r : Journal.row) ->
              Printf.printf "%4d %-7s %-10s %-22s in=%-6d cached=%-6d write=%-6d out=%-5d %6dms $%.6f %s%s\n"
                r.Journal.seq (Journal.kind_name r.Journal.kind) r.Journal.provider r.Journal.model
                r.Journal.usage.Wire.input r.Journal.usage.Wire.cache_read
                r.Journal.usage.Wire.cache_write r.Journal.usage.Wire.output r.Journal.latency_ms
                r.Journal.cost_usd r.Journal.outcome
                (if r.Journal.ref_ = "" then "" else " ref=" ^ r.Journal.ref_);
              if r.Journal.detail <> "" then Printf.printf "       %s\n" (substr r.Journal.detail 200))
            (List.filteri (fun i _ -> i >= total - n) rows);
          Printf.printf "%d rows\n" total;
          List.iter print_endline (Journal.hit_rates rows);
          0)

let verdict config ref_ status note =
  match Config.load config with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok cfg ->
      let r : Journal.row =
        {
          Journal.seq = Journal.next_seq cfg.Config.journal;
          ts = Unix.gettimeofday ();
          kind = Journal.Verdict;
          identity = cfg.Config.identity;
          task = "";
          model = "";
          provider = "";
          prefix_hash = "";
          prefix_tokens = 0;
          usage = Wire.usage_zero;
          cost_usd = 0.0;
          latency_ms = 0;
          route = "";
          outcome = status;
          detail = note;
          ref_ = string_of_int ref_;
        }
      in
      (match Journal.append ~path:cfg.Config.journal r with
      | Ok () -> Printf.printf "verdict %s on row %d\n" status ref_; 0
      | Error m -> prerr_endline ("erde: " ^ m); 1)

let get url =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Http_client.request ~env ~sw ~meth:"GET" ~url ~headers:[ ("accept", "application/json") ] () with
  | Error m -> prerr_endline ("erde: " ^ m); 1
  | Ok (status, body) ->
      Printf.printf "http %d, %d bytes\n%s\n" status (String.length body) (substr body 600);
      if status >= 200 && status < 300 then 0 else 2

let cmd_providers = Cmd.v (Cmd.info "providers" ~doc:"List providers, routes, authority and grants (never prints secrets).")
    Term.(const providers $ config_term)

let cmd_plan =
  Cmd.v (Cmd.info "plan" ~doc:"Dry run: segments, prefix hash, cache plan, routing. Spends nothing.")
    Term.(const plan $ config_term $ model_term $ task_term $ claims_term $ claims_mode_term $ project_term)

let cmd_claims =
  Cmd.v (Cmd.info "claims" ~doc:"Fetch an identity's claims from tuna and render them into segments.")
    Term.(const claims $ config_term $ prefix_term $ limit_term $ claims_mode_term)

let cmd_tool =
  Cmd.v (Cmd.info "tool" ~doc:"Call one tool through the grant gate (dry: no model, no tokens).")
    Term.(const tool $ config_term $ prim_term $ path_term $ arg_term)

let cmd_turn =
  Cmd.v (Cmd.info "turn" ~doc:"Run one microturn (bounded tool loop) and journal every step.")
    Term.(
      const turn $ config_term $ model_term $ task_term $ max_tokens_term $ max_steps_term $ warm_term
      $ stream_term $ claims_mode_term $ claims_limit_term)

let cmd_journal = Cmd.v (Cmd.info "journal" ~doc:"Tail the journal and print the cache hit rates.")
    Term.(const journal $ config_term $ n_term)

let cmd_verdict =
  Cmd.v (Cmd.info "verdict" ~doc:"Record a verdict correcting an earlier row (rows are facts, verdicts are corrections).")
    Term.(const verdict $ config_term $ ref_term $ status_term $ note_term)

let interval_term =
  Arg.(value & opt int 900 & info [ "interval" ] ~docv:"SECONDS" ~doc:"Seconds between loop ticks.")

let max_ticks_term =
  Arg.(value & opt int 3 & info [ "max-ticks" ] ~docv:"N" ~doc:"Ticks before the loop stops.")

let task_path_term =
  Arg.(value & opt string "u.gregor.loop.task" & info [ "task-path" ] ~docv:"PATH" ~doc:"Claim holding the standing task.")

let max_tokens_term =
  Arg.(value & opt int 1024 & info [ "max-tokens" ] ~docv:"N" ~doc:"Output token ceiling per model call.")

let loop config model interval max_ticks task_path loop_max_tokens claims_mode claims_limit =
  let fallback =
    "loop tick: no readable standing task at " ^ task_path
    ^ "; write a one-line status claim under u.gregor.loop.log.<epoch-utc> (tree.put value_text), then stop."
  in
  for n = 1 to max_ticks do
    Printf.printf "== tick %d/%d (%.0f)\n" n max_ticks (Unix.time ());
    let task =
      let task = ref fallback in
      let _code =
        with_authority config (fun _cfg auth ->
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            match Authority.tree_get ~env ~sw auth ~path:task_path with
            | Ok (tern, _, _) -> (
                match Treecalc.from_ternary tern with
                | Ok v -> task := (match Treecalc.as_chunked_text v with Some s -> s | None -> fallback)
                | Error _ -> task := fallback);
            0
            | Error _ -> task := fallback; 0)
      in
      !task
    in
    Printf.printf "task %s\n" (substr task 80);
    let _code = turn config model task loop_max_tokens 6 "" false claims_mode claims_limit in
    if n < max_ticks then (
      Printf.printf "-- sleeping %ds\n" interval;
      Unix.sleep interval)
  done;
  print_endline "loop done";
  0

let cmd_loop =
  Cmd.v (Cmd.info "loop" ~doc:"Run the standing task claim on an interval (the identity heartbeat).")
      Term.(
        const loop $ config_term $ model_term $ interval_term $ max_ticks_term $ task_path_term $ max_tokens_term
          $ claims_mode_term $ claims_limit_term)

let et text = print_endline (Treecalc.to_ternary (Treecalc.chunked_text text)); 0

let cmd_et =
  Cmd.v (Cmd.info "et" ~doc:"Encode text to the chunked-claim ternary (for seeding claims from a shell).")
    Term.(const et $ Arg.(required & pos 0 (some string) None & info [] ~docv:"TEXT"))

let cmd_get =
  Cmd.v (Cmd.info "get" ~doc:"Fetch a URL and print the status (the TLS/HTTP smoke test).")
    Term.(const get $ url_term)

let tc ternary =
  match Treecalc.from_ternary ternary with
  | Error e -> Printf.printf "error %s\n" e; 1
  | Ok v ->
      Printf.printf "canonical %s\n" (Treecalc.to_ternary v);
      Printf.printf "text %s\n" (Treecalc.to_text v);
      (match Treecalc.as_chunked_text v with Some s -> Printf.printf "chunked %s\n" s | None -> print_endline "chunked -");
      (match Treecalc.as_cons_text v with Some s -> Printf.printf "cons %s\n" s | None -> print_endline "cons -");
      (match Treecalc.as_text v with Some s -> Printf.printf "chars %s\n" s | None -> print_endline "chars -");
      (match Treecalc.eval (match v with Treecalc.T ks -> ks) with
       | Ok (vv, steps) ->
           Printf.printf "reduce %d steps -> %s\n" steps (Treecalc.to_ternary (Treecalc.of_kids vv));
           (match Treecalc.as_text (Treecalc.of_kids vv) with Some s -> Printf.printf "chars-after-reduce %s\n" s | None -> ())
       | Error e -> Printf.printf "reduce - %s\n" e);
      0

let cmd_tc =
  Cmd.v (Cmd.info "tc" ~doc:"Decode a ternary value: canonical, text, char attempt, one full reduction.")
    Term.(const tc $ Arg.(required & pos 0 (some string) None & info [] ~docv:"TERNARY"))

(* The independent re-verification the harness brief promises: a claim cites a
   hash; here we rebuild both sides locally and make the match a typed result. *)
let verify config path =
  with_authority config (fun cfg auth ->
      ignore cfg;
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match Authority.tree_get ~env ~sw auth ~path with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok (tern, hash, version) ->
          let computed = Treecalc.sha256_of_string tern in
          let decode_ok =
            match Treecalc.from_ternary tern with Ok _ -> true | Error _ -> false
          in
          Printf.printf "path        %s\n" path;
          Printf.printf "version     %d\n" version;
          Printf.printf "ternary     %d canonical digits\n" (String.length tern);
          Printf.printf "stored      %s\n" hash;
          Printf.printf "recomputed  %s\n" computed;
          Printf.printf "decode      %s\n" (if decode_ok then "ok" else "FAIL");
          if hash = "" then (
            print_endline "verdict     no stored hash to compare";
            1)
          else if hash = computed && decode_ok then (
            print_endline "verdict     agree";
            0)
            else (
              print_endline "verdict     MISMATCH";
              1))

let cmd_verify =
  Cmd.v (Cmd.info "verify" ~doc:"Independently re-verify a claim: recompute the citation hash and decode the value.")
    Term.(const verify $ config_term $ Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"))

(* Deriv receipt re-execution, run-4 verified (ENTRY-RECIPE): a deriv claim's
   chunked text carries the program, its whole-value args, and the expected
   result; re-execute = ONE flat-spine eval Treecalc.eval (to_kids prog @ args),
   byte-exact against expect. Format (one token per ternary, args optional):
     prog <canonical ternary> [args <ternary> <ternary> ...] expect <ternary> *)
let split_ws s =
  String.split_on_char '\n' s
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (fun f -> f <> "")

let idx name fields =
  let rec go i = function
    | [] -> None
    | h :: r -> if h = name then Some i else go (i + 1) r
  in
  go 0 fields

let deriv config path =
  with_authority config (fun cfg auth ->
      ignore cfg;
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match Authority.tree_get ~env ~sw auth ~path with
      | Error m -> prerr_endline ("erde: " ^ m); 1
      | Ok (tern, hash, version) ->
          let computed = Treecalc.sha256_of_string tern in
          Printf.printf "path        %s\n" path;
          Printf.printf "version     %d\n" version;
          Printf.printf "stored      %s\n" hash;
          Printf.printf "recomputed  %s\n" computed;
          Printf.printf "hash        %s\n"
            (if hash = "" then "no stored hash"
             else if hash = computed then "agree" else "MISMATCH");
          (match Treecalc.from_ternary tern with
           | Error e -> Printf.printf "verdict     bad value ternary (%s)\n" e; 1
           | Ok tv -> (
               match Treecalc.as_chunked_text tv with
               | None -> print_endline "verdict     no deriv recipe (value is not chunked text)"; 1
               | Some text -> (
                   let fields = split_ws text in
                   let after t =
                     Option.bind (idx t fields) (fun i -> List.nth_opt fields (i + 1))
                   in
                   let args, expect =
                     (match idx "args" fields with
                      | Some i ->
                          let j =
                            Option.value ~default:(List.length fields) (idx "expect" fields)
                          in
                            (Some (List.filteri (fun k _ -> (k > i) && (k < j)) fields), after "expect")
                      | None -> (Some [], after "expect"))
                   in
                   match (after "prog", args, expect) with
                   | Some p, Some argv, Some e -> (
                       let tern_of label s =
                         match Treecalc.from_ternary s with
                         | Ok v -> Ok v
                         | Error msg ->
                             Error
                               (Printf.sprintf "verdict     bad %s ternary (%s)" label msg)
                       in
                       let ba =
                         List.fold_left
                           (fun acc a ->
                             match acc with
                             | Ok l ->
                                 (match tern_of "arg" a with
                                  | Ok v -> Ok (l @ [ v ])
                                  | Error em -> Error em)
                             | Error em -> Error em)
                           (Ok []) argv
                       in
                       (match (tern_of "prog" p, ba, tern_of "expect" e) with
                        | Ok pval, Ok avals, Ok _ ->
                            Printf.printf "re-exec     %d arg(s), one flat-spine eval\n"
                              (List.length avals);
                            (match Treecalc.eval (Treecalc.to_kids pval @ avals) with
                             | Ok (v, steps) ->
                                 let result = Treecalc.to_ternary (Treecalc.of_kids v) in
                                 Printf.printf "result      %s (%d steps)\n" result steps;
                                 if result = e then (print_endline "verdict     deriv agree"; 0)
                                 else (print_endline "verdict     deriv MISMATCH"; 1)
                             | Error msg ->
                                 print_endline ("verdict     re-exec failed (" ^ msg ^ ")");
                                 1)
                        | Error msg, _, _ | _, Error msg, _ | _, _, Error msg ->
                            print_endline msg; 1))
                   | _ ->
                       print_endline
                         "verdict     no deriv recipe (prog/expect lines missing)";
                       1))))

let cmd_deriv =
  Cmd.v
    (Cmd.info "deriv"
       ~doc:
         "Re-execute a deriv receipt claim: hash check + one flat-spine eval, \"verdict\" = verbatim output text")
    Term.(const deriv $ config_term $ Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH"))

let cmd_put =
  Cmd.v (Cmd.info "put" ~doc:"Write a value_text claim through the same grant gate and encoding the model's tree.put uses.")
    Term.(
      const put $ config_term
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"TEXT"))

(* Raw-ternary put for program artifacts: the value IS canonical ternary,
   not text to chunk-encode. Same grant gate. *)
let putr config path ternary =
  with_authority config (fun cfg auth ->
      ignore cfg;
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        Tools.dispatch ~env ~sw ~authority:auth ~grants:cfg.Config.grants
          {
            Tools.prim = "tree.put";
            args = `Assoc [ ("path", `String path); ("value_ternary", `String ternary) ]
          }
      with
      | Ok j ->
          Printf.printf "%s\n" (Yojson.Safe.pretty_to_string j);
          0
      | Error m ->
          Printf.printf "refused: %s\n" m;
          1)

let cmd_putr =
  Cmd.v (Cmd.info "putr" ~doc:"Write a RAW ternary claim (program artifacts): value stored as-is, same grant gate.")
    Term.(
      const putr $ config_term
      $ Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH")
      $ Arg.(required & pos 1 (some string) None & info [] ~docv:"TERNARY"))

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "erde" ~version:"0.2"
             ~doc:"Erde Lang: a substrate around tuna. One microturn, journaled, cache-aware.")
            [ cmd_providers; cmd_plan; cmd_claims; cmd_tool; cmd_put; cmd_putr; cmd_turn; cmd_journal; cmd_verdict; cmd_get; cmd_tc; cmd_verify; cmd_deriv; cmd_loop; cmd_et ]))
