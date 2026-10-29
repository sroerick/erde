(* Erde.Turn -- one microturn: prompt in, work out.

   The unit is not a conversation turn.  A microturn is: choose a provider
   (route), plan the cache, render the segments, spend one model call with a
   grant-sized budget, and journal what it cost and where it ran.  Nothing
   here owns state that survives the call: continuity lives in claims.

   Tool calls come back as typed JSON, never as a shell string.

   [handle_response] is separated from [run] so the success path can be
   tested against a recorded body instead of a live provider. *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

type outcome = {
  model : string;
  provider : string;
  route : string;
  plan : Cache_strategy.plan;
  response : Wire.response;
  cost_usd : float;
  latency_ms : int;
  raw : string;
}

let path_for (p : Config.provider) =
  match p.Config.wire with
  | Config.Anthropic -> "/v1/messages"
  | Config.Openai -> "/v1/chat/completions"

let auth_headers (p : Config.provider) key =
  [
    (p.Config.auth_header, p.Config.auth_scheme ^ key);
    ("content-type", "application/json");
    ("accept", "application/json");
  ]

let row ~cfg ~plan ~provider ~model ~task ~route ~usage ~cost ~latency_ms ~outcome =
  {
    Journal.seq = Journal.next_seq cfg.Config.journal;
    ts = Unix.gettimeofday ();
    kind = Journal.Call;
    identity = cfg.Config.identity;
    task;
    model;
    provider;
    prefix_hash = plan.Cache_strategy.head;
    prefix_tokens = plan.Cache_strategy.prefix_tokens;
    usage;
    cost_usd = cost;
    latency_ms;
    route;
    outcome;
    ref_ = "";
  }

let record cfg r =
  match Journal.append ~path:cfg.Config.journal r with
  | Ok () -> ()
  | Error m -> prerr_endline ("erde: journal write failed: " ^ m)

let handle_response ~cfg ~(p : Config.provider) ~plan ~model ~task ~route ~latency_ms ~status
    ~raw =
  if status < 200 || status >= 300 then begin
    let body = String.sub raw 0 (min 400 (String.length raw)) in
    record cfg
      (row ~cfg ~plan ~provider:p.Config.name ~model ~task ~route ~usage:Wire.usage_zero
         ~cost:0.0 ~latency_ms ~outcome:(Printf.sprintf "http %d" status));
    Error (Printf.sprintf "provider %s: http %d: %s" p.Config.name status (Config.redact cfg body))
  end
  else
    match Yojson.Safe.from_string raw with
    | exception Yojson.Json_error m ->
        Error (Printf.sprintf "provider %s: unparseable body: %s" p.Config.name m)
    | json -> (
        match Wire.parse_response p json with
        | Error m -> Error m
        | Ok response ->
            let cost = Wire.cost_usd p response.Wire.usage in
            let outcome = match response.Wire.stop with Some s -> s | None -> "ok" in
            record cfg
              (row ~cfg ~plan ~provider:p.Config.name ~model ~task ~route
                 ~usage:response.Wire.usage ~cost ~latency_ms ~outcome);
            Ok
              {
                model;
                provider = p.Config.name;
                route;
                plan;
                response;
                cost_usd = cost;
                latency_ms;
                raw;
              })

let run ~env ~sw (cfg : Config.t) ~model ~system (segs : Segments.t) ~task ~max_tokens
    ?(tools = []) ?warm () =
  let* p, route = Config.choose cfg ?warm ~model () in
  let* key = Config.key cfg p in
  let plan = Cache_strategy.plan p segs in
  let body =
    Wire.build_request p plan ~model ~system ~messages:[] ~max_tokens ~tools
    |> Yojson.Safe.to_string
  in
  let url = p.Config.base_url ^ path_for p in
  let t0 = Unix.gettimeofday () in
  let* status, raw =
    Http_client.request ~env ~sw ~meth:"POST" ~url ~headers:(auth_headers p key) ~body ()
  in
  let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
  handle_response ~cfg ~p ~plan ~model ~task ~route ~latency_ms ~status ~raw
