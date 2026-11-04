(* Erde.Turn -- one microturn: prompt in, work out.

   The unit is not a conversation turn.  A microturn is: choose a provider
   (route), plan the cache, render the segments, spend one model call with a
   grant-sized budget, run whatever tools it asks for (each one gated before
   it leaves the process), and journal every step.

   Boundaries, deliberately:
     - nothing here owns state that survives the call; continuity lives in
       claims;
     - tool calls come back as typed JSON, never as a shell string;
     - a refused tool call is a journal row, not a provider round trip. *)

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

type step = {
  ordinal : int;
  prim : string;
  args : Yojson.Safe.t;
  allowed : bool;
  result : string;
}

type run_result = {
  outcome : outcome option;  (* None only when the step budget ran out first *)
  steps : step list;
  calls : int;
}

(* The endpoint is a provider property, not a wire constant: whatever sits in
   front of the model may serve its own prefix (api.synthetic.new/openai/v1 is
   a base that already ends in the version), so the provider row says where the
   call goes. *)

let auth_headers (p : Config.provider) key =
  [
    (p.Config.auth_header, p.Config.auth_scheme ^ key);
    ("content-type", "application/json");
    ("accept", "application/json");
  ]

let row ~kind ~cfg ~plan ~provider ~model ~task ~route ~usage ~cost ~latency_ms ~outcome
    ~detail =
  {
    Journal.seq = Journal.next_seq cfg.Config.journal;
    ts = Unix.gettimeofday ();
    kind;
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
    detail;
    ref_ = "";
  }

let record cfg r =
  match Journal.append ~path:cfg.Config.journal r with
  | Ok () -> ()
  | Error m -> prerr_endline ("erde: journal write failed: " ^ m)

let truncate n s = if String.length s <= n then s else String.sub s 0 n ^ "..."

let contains_substring (s : string) (m : string) =
  let n = String.length s and k = String.length m in
  let rec go i = i + k <= n && ((String.sub s i k) = m || go (i + 1)) in
  go 0

(* The parsing + journalling seam, so the success path can be tested against
   a recorded body instead of a live provider. *)
let handle_response ~cfg ~(p : Config.provider) ~plan ~model ~task ~route ~latency_ms ~status
    ~raw =
  if status < 200 || status >= 300 then begin
    record cfg
      (row ~kind:Journal.Call ~cfg ~plan ~provider:p.Config.name ~model ~task ~route
         ~usage:Wire.usage_zero ~cost:0.0 ~latency_ms
         ~outcome:(Printf.sprintf "http %d" status) ~detail:"");
    Error
      (Printf.sprintf "provider %s: http %d: %s" p.Config.name status
         (Config.redact cfg (truncate 400 raw)))
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
              (row ~kind:Journal.Call ~cfg ~plan ~provider:p.Config.name ~model ~task ~route
                 ~usage:response.Wire.usage ~cost ~latency_ms ~outcome ~detail:"");
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

(* The streaming path: same journal row, same accounting, assembled from the
   transcript instead of parsed from one body. *)
let handle_stream_response ~cfg ~(p : Config.provider) ~plan ~model ~task ~route ~latency_ms
    ~status ~raw =
  if status < 200 || status >= 300 then
    handle_response ~cfg ~p ~plan ~model ~task ~route ~latency_ms ~status ~raw
  else
    let response = Wire.assemble_stream p raw in
    let cost = Wire.cost_usd p response.Wire.usage in
    let outcome = match response.Wire.stop with Some s -> s | None -> "ok" in
    record cfg
      (row ~kind:Journal.Call ~cfg ~plan ~provider:p.Config.name ~model ~task ~route
         ~usage:response.Wire.usage ~cost ~latency_ms ~outcome
         ~detail:(Printf.sprintf "streamed %d bytes" (String.length raw)));
    Ok { model; provider = p.Config.name; route; plan; response; cost_usd = cost; latency_ms; raw }

let one_call ?(stream = false) ~env ~sw ~cfg ~model ~system segs ~task ~max_tokens ~tools
    ~messages ?warm () =
  let* p, route = Config.choose cfg ?warm ~model () in
  let* key = Config.key cfg p in
  let plan = Cache_strategy.plan p segs in
  (* the caller may pass explicit schemas; otherwise the host's own are used,
     shaped for whichever wire the route chose *)
  let tools = if tools = [] then Tools.schemas p.Config.wire else tools in
  let body =
    Wire.build_request ~stream p plan ~model ~system ~messages ~max_tokens ~tools
    |> Yojson.Safe.to_string
  in
  let url = p.Config.base_url ^ p.Config.path in
  let t0 = Unix.gettimeofday () in
    let* status, raw =
      Retry.request ~clock:(Eio.Stdenv.clock env)
        ~label:(Printf.sprintf "%s %s" p.Config.name model)
        (fun () ->
          Http_client.request ~env ~sw ~meth:"POST" ~url ~headers:(auth_headers p key) ~body ())
    in
  let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
  if stream then handle_stream_response ~cfg ~p ~plan ~model ~task ~route ~latency_ms ~status ~raw
  else handle_response ~cfg ~p ~plan ~model ~task ~route ~latency_ms ~status ~raw

(* Every tool call gets a row, allowed or not: "keep many attempts in flight
   and trust none of them" only works if the trust decision is recorded. *)
let execute_calls ~env ~sw ~cfg ~authority ~grants ~plan ~provider ~model ~task ~route
    ~ordinal_start calls =
  let rec go i acc = function
    | [] -> List.rev acc
    | (c : Wire.tool_call) :: rest ->
        let call = { Tools.prim = c.Wire.name; args = c.Wire.args } in
        let step =
          match Tools.dispatch ~env ~sw ~authority ~grants call with
          | Ok result ->
              {
                ordinal = i;
                prim = c.Wire.name;
                args = c.Wire.args;
                allowed = true;
                result = Yojson.Safe.to_string result;
              }
          | Error m ->
              { ordinal = i; prim = c.Wire.name; args = c.Wire.args; allowed = false; result = m }
        in
        let detail =
          truncate 600
            (Printf.sprintf "%s %s -> %s" step.prim (Yojson.Safe.to_string step.args)
               (if step.allowed then step.result else "REFUSED " ^ step.result))
        in
        record cfg
          (row ~kind:Journal.Tool ~cfg ~plan ~provider ~model ~task ~route
             ~usage:Wire.usage_zero ~cost:0.0 ~latency_ms:0
             ~outcome:(if step.allowed then "ok" else "denied") ~detail);
        go (i + 1) (step :: acc) rest
  in
  go ordinal_start [] calls

let results_message steps =
  steps
  |> List.map (fun s ->
         if s.allowed then Printf.sprintf "tool %s -> %s" s.prim s.result
         else Printf.sprintf "tool %s -> REFUSED: %s" s.prim s.result)
  |> String.concat "\n"

(* The bounded loop: a microturn may take up to [max_steps] model calls, and
   stops as soon as the model stops asking for tools. *)
let run ~env ~sw ~cfg ~authority ~grants ~model ~system segs ~task ~max_tokens
    ?(tools = []) ?(max_steps = 3) ?(stream = false) ?warm () =
  let rec loop n messages steps =
    if n > max_steps then Ok { outcome = None; steps = List.rev steps; calls = n - 1 }
    else
      match
        one_call ~stream ~env ~sw ~cfg ~model ~system segs ~task ~max_tokens ~tools ~messages
          ?warm ()
      with
      | Error m -> Error m
      | Ok outcome -> (
          match outcome.response.Wire.tool_calls with
          | [] ->
              (* deepseek on gomodel sometimes escapes the function-call
                 channel and emits its raw DSML chat-template markup as text
                 (observed: out=9..13, text is only closing tags).  That is
                 never a real answer; bounce it back to the function channel
                 while the step budget has room. *)
              (match outcome.response.Wire.text with
              | Some t when n < max_steps && contains_substring t "DSML" ->
                  loop (n + 1)
                    (messages
                    @ [
                        ("assistant", truncate 120 t);
                        ( "user",
                          "Your tool call arrived as raw text markup instead of a\nfunction call. Re-issue it through the function-call channel." );
                      ])
                    steps
              | _ -> Ok { outcome = Some outcome; steps = List.rev steps; calls = n })
            | calls ->
              let executed =
                execute_calls ~env ~sw ~cfg ~authority ~grants ~plan:outcome.plan
                  ~provider:outcome.provider ~model ~task ~route:outcome.route
                  ~ordinal_start:n calls
              in
              let mutter = String.concat "," (List.map (fun c -> c.Wire.name) calls) in
              let messages =
                messages
                @ [
                    ("assistant", "tool calls: " ^ mutter);
                    ("user", results_message executed);
                  ]
              in
              loop (n + 1) messages (List.rev_append executed steps))
  in
  loop 1 [] []
