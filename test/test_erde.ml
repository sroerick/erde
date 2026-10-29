open Erde

let with_tmp f =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "erde-test-%d-%d" (Unix.getpid ()) (Random.int 1_000_000))
  in
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  nl = 0
  || (let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
      go 0)

let write path text =
  let oc = open_out_bin path in
  output_string oc text;
  close_out oc

let seg kind id text : Segments.segment = { Segments.kind; id; text }

(* ---------- segments ---------- *)

let test_order_enforced () =
  let bad = [ seg Segments.Task "t" "do"; seg Segments.Claims "c" "cl" ] in
  match Segments.make bad with
  | Ok _ -> Alcotest.fail "task before claims must be rejected"
  | Error m -> Alcotest.(check bool) "names the order" true (String.length m > 0)

let test_hash_stable_under_claim_reorder () =
  let a = [ { Segments.Claim_set.path = "u.x"; value = "1"; source = "tuna" } ] in
  let b =
    [
      { Segments.Claim_set.path = "u.y"; value = "2"; source = "tuna" };
      { Segments.Claim_set.path = "u.x"; value = "1"; source = "tuna" };
    ]
  in
  let c = List.rev b in
  Alcotest.(check string) "same set, same hash" (Segments.Claim_set.hash b) (Segments.Claim_set.hash c);
  ignore a

let test_early_edit_moves_later_boundaries () =
  let mk head =
    match
      Segments.make [ seg Segments.Framework "f" head; seg Segments.Claims "c" "same"; seg Segments.Task "t" "same" ]
    with
    | Ok t -> t
    | Error _ -> Alcotest.fail "segments should be ordered"
  in
  let h1 = Segments.boundaries (mk "policy A") and h2 = Segments.boundaries (mk "policy B") in
  let index2 l = List.nth l 1 and index3 l = List.nth l 2 in
  let (_, _, a1, _) = index2 h1 and (_, _, b1, _) = index2 h2 in
  let (_, _, a2, _) = index3 h1 and (_, _, b2, _) = index3 h2 in
  Alcotest.(check bool) "claims boundary moves" true (a1 <> b1);
  Alcotest.(check bool) "task boundary moves" true (a2 <> b2)

(* ---------- cache strategy ---------- *)

let provider ?(mode = Config.Implicit) ?(min = 1024) ?(live = true) name =
  {
    Config.name;
    wire = Config.Openai;
    base_url = "http://localhost:1";
    key_env = "K";
    auth_header = "Authorization";
    auth_scheme = "Bearer ";
    prices = { Config.input = 1.0; output = 2.0; cache_write_mult = 1.25; cache_read_mult = 0.1 };
    cache = { Config.mode; ttl_s = 300; min_prefix_tokens = min };
    live;
  }

let big_segments () =
  let pad = String.make 4000 'x' in
  match
    Segments.make
      [ seg Segments.Framework "f" pad; seg Segments.Claims "c" pad; seg Segments.Task "t" "task" ]
  with
  | Ok t -> t
  | Error _ -> Alcotest.fail "ordered"

let test_implicit_has_no_breakpoints () =
  let p = Cache_strategy.plan (provider "openai-ish") (big_segments ()) in
  Alcotest.(check int) "no breakpoints" 0 (List.length p.Cache_strategy.breakpoints);
  Alcotest.(check bool) "implicit" true (p.Cache_strategy.mode = Config.Implicit)

let test_explicit_below_minimum_places_none () =
  let p = Cache_strategy.plan (provider ~mode:Config.Explicit ~min:100_000 "small") (big_segments ()) in
  Alcotest.(check int) "no breakpoints under the minimum" 0 (List.length p.Cache_strategy.breakpoints);
  Alcotest.(check bool) "explains why" true
    (String.length p.Cache_strategy.reason > 0
    && String.contains p.Cache_strategy.reason 'm')

let test_explicit_above_minimum_breaks_at_shared_boundaries () =
  let p = Cache_strategy.plan (provider ~mode:Config.Explicit ~min:1024 "big") (big_segments ()) in
  Alcotest.(check int) "two shared segments yield two breakpoints" 2
    (List.length p.Cache_strategy.breakpoints);
  Alcotest.(check (list int)) "framework and claims boundaries" [ 0; 1 ]
    p.Cache_strategy.breakpoints

(* ---------- wire ---------- *)

let anthropic_json =
  {|{"id":"msg_1","type":"message","role":"assistant","content":[{"type":"text","text":"ok"}],
     "stop_reason":"end_turn",
     "usage":{"input_tokens":100,"output_tokens":40,"cache_creation_input_tokens":2000,"cache_read_input_tokens":8000}}|}

let openai_json =
  {|{"id":"c1","choices":[{"index":0,"finish_reason":"tool_calls","message":{"role":"assistant","content":null,
     "tool_calls":[{"id":"t1","type":"function","function":{"name":"tree.put","arguments":"{\"path\":\"u.g.notes\",\"value\":\"hi\"}"}}]}}],
     "usage":{"prompt_tokens":5000,"completion_tokens":120,"prompt_tokens_details":{"cached_tokens":4096}}}|}

let anthropic_provider () =
  { (provider ~mode:Config.Explicit "anthropic") with
    Config.wire = Config.Anthropic;
    prices = { Config.input = 3.0; output = 15.0; cache_write_mult = 1.25; cache_read_mult = 0.1 };
  }

let test_anthropic_usage_and_cost () =
  let p = anthropic_provider () in
  let r = Wire.parse_response p (Yojson.Safe.from_string anthropic_json) in
  match r with
  | Error m -> Alcotest.fail m
  | Ok resp ->
      Alcotest.(check int) "input excludes cache" 100 resp.Wire.usage.Wire.input;
      Alcotest.(check int) "cache write" 2000 resp.Wire.usage.Wire.cache_write;
      Alcotest.(check int) "cache read" 8000 resp.Wire.usage.Wire.cache_read;
      Alcotest.(check (float 1e-9)) "cost" 0.0108 (Wire.cost_usd p resp.Wire.usage);
      Alcotest.(check bool) "text" true (resp.Wire.text = Some "ok")

let test_openai_usage_and_cost () =
  let p = { (provider "gomodel") with Config.prices = { Config.input = 2.0; output = 8.0; cache_write_mult = 1.25; cache_read_mult = 0.1 } } in
  match Wire.parse_response p (Yojson.Safe.from_string openai_json) with
  | Error m -> Alcotest.fail m
  | Ok resp ->
      Alcotest.(check int) "cached is a subset of prompt" 904 resp.Wire.usage.Wire.input;
      Alcotest.(check int) "no write charge on this wire" 0 resp.Wire.usage.Wire.cache_write;
      Alcotest.(check (float 1e-9)) "cost" 0.0035872 (Wire.cost_usd p resp.Wire.usage);
      (match resp.Wire.tool_calls with
      | [ tc ] ->
          Alcotest.(check string) "tool name" "tree.put" tc.Wire.name;
          Alcotest.(check string) "args parsed as json, not a string"
            {|{"path":"u.g.notes","value":"hi"}|}
            (Yojson.Safe.to_string tc.Wire.args)
      | _ -> Alcotest.fail "expected one tool call")

let test_anthropic_request_places_breakpoints () =
  let p = anthropic_provider () in
  let segs = big_segments () in
  let plan = Cache_strategy.plan p segs in
  let blocks = Wire.anthropic_system_blocks segs plan in
  let has_cache_control i =
    match List.nth blocks i with
    | `Assoc kvs -> List.mem_assoc "cache_control" kvs
    | _ -> false
  in
  Alcotest.(check bool) "first breakpoint marked" true (has_cache_control 0);
  Alcotest.(check bool) "last segment unmarked" false (has_cache_control (List.length blocks - 1))

(* ---------- routing ---------- *)

let routed_config () =
  {
    Config.path = "inline";
    journal = "/dev/null";
    identity = "t";
    providers = [ provider "gomodel"; provider ~live:false "neuralwatt"; provider "synthetic" ];
    routes = [ { Config.model = "glm"; candidates = [ "gomodel"; "neuralwatt"; "synthetic" ] } ];
    env = [];
  }

let test_retired_candidate_is_dropped () =
  let cfg = routed_config () in
  match Config.choose cfg ~model:"glm" () with
  | Error m -> Alcotest.fail m
  | Ok (p, reason) ->
      Alcotest.(check string) "live candidate chosen" "gomodel" p.Config.name;
      Alcotest.(check bool) "names the retirement" true
        (String.length reason > 0 && String.contains reason 'n')

let test_warm_provider_wins () =
  match Config.choose (routed_config ()) ~warm:"synthetic" ~model:"glm" () with
  | Error m -> Alcotest.fail m
  | Ok (p, reason) ->
      Alcotest.(check string) "warm provider kept" "synthetic" p.Config.name;
      Alcotest.(check bool) "reason says warm" true
        (String.length reason >= 4 && String.sub reason 0 4 = "warm")

let test_unknown_model_errors () =
  match Config.choose (routed_config ()) ~model:"nope" () with
  | Ok _ -> Alcotest.fail "unknown model must not route"
  | Error _ -> ()

(* ---------- journal ---------- *)

let sample_row seq provider =
  {
    Journal.seq;
    ts = 1.0;
    kind = Journal.Call;
    identity = "gregor";
    task = "t";
    model = "m";
    provider;
    prefix_hash = "abc";
    prefix_tokens = 2000;
    usage = { Wire.input = 100; output = 10; cache_write = 0; cache_read = 900 };
    cost_usd = 0.5;
    latency_ms = 12;
    route = "cold";
    outcome = "end_turn";
    ref_ = "";
  }

let test_journal_round_trip () =
  with_tmp (fun dir ->
      let path = Filename.concat dir "j.jsonl" in
      (match Journal.append ~path (sample_row 0 "gomodel") with
      | Ok () -> ()
      | Error m -> Alcotest.fail m);
      (match Journal.append ~path (sample_row 1 "anthropic") with
      | Ok () -> ()
      | Error m -> Alcotest.fail m);
      match Journal.read path with
      | Error m -> Alcotest.fail m
      | Ok rows ->
          Alcotest.(check int) "two rows" 2 (List.length rows);
          Alcotest.(check int) "cache read survives" 900
            (List.nth rows 0).Journal.usage.Wire.cache_read;
          Alcotest.(check (float 1e-9)) "cost survives" 0.5 (List.nth rows 1).Journal.cost_usd;
          Alcotest.(check int) "next seq" 2 (Journal.next_seq path);
          Alcotest.(check int) "one hit-rate line per provider" 2
            (List.length (Journal.hit_rates rows)))

(* ---------- sse ---------- *)

let test_sse_parse () =
  let body =
    ": keep-alive\n\nevent: message\ndata: {\"a\":1}\n\ndata: {\"b\":}\ndata: 2\n\n"
  in
  match Sse.parse body with
  | [ e1; e2 ] ->
      Alcotest.(check string) "named event" {|{"a":1}|} e1.Sse.data;
      Alcotest.(check bool) "event name" true (e1.Sse.name = Some "message");
      Alcotest.(check string) "multiline data joined" "{\"b\":}\n2" e2.Sse.data
  | other -> Alcotest.failf "expected 2 events, got %d" (List.length other)

(* ---------- config ---------- *)

let test_env_parse () =
  let m =
    Config.parse_env
      "# comment\nexport A=1\nB = \"two words\"\nC='three'\n\nEMPTY=\n"
  in
  Alcotest.(check (option string)) "export tolerated" (Some "1") (List.assoc_opt "A" m);
  Alcotest.(check (option string)) "quotes stripped" (Some "two words") (List.assoc_opt "B" m);
  Alcotest.(check (option string)) "single quotes" (Some "three") (List.assoc_opt "C" m);
  Alcotest.(check (option string)) "empty value" (Some "") (List.assoc_opt "EMPTY" m)

let test_env_file_mode_is_enforced () =
  with_tmp (fun dir ->
      let path = Filename.concat dir "llm.env" in
      write path "K=v\n";
      Unix.chmod path 0o644;
      (match Config.load_env_file path with
      | Ok _ -> Alcotest.fail "a world-readable secrets file must be refused"
      | Error m ->
          Alcotest.(check bool) "message names the mode" true
            (String.length m > 0 && String.contains m '6'));
      Unix.chmod path 0o600;
      match Config.load_env_file path with
      | Ok m -> Alcotest.(check (option string)) "loaded" (Some "v") (List.assoc_opt "K" m)
      | Error m -> Alcotest.fail m)

let config_json =
  {|{"identity":"erde-gateway","journal":"journal.jsonl",
     "providers":[{"name":"gomodel","wire":"openai","base_url":"http://127.0.0.1:1","key_env":"GOMODEL_API_KEY",
                   "cache_mode":"implicit","input_price":0.6,"output_price":2.4}],
     "routes":[{"model":"glm","candidates":["gomodel"]}]}|}

let test_config_load () =
  with_tmp (fun dir ->
      let path = Filename.concat dir "erde.json" in
      write path config_json;
      write (Filename.concat dir "llm.env") "GOMODEL_API_KEY=secret-value-here\n";
      Unix.chmod (Filename.concat dir "llm.env") 0o600;
      match Config.load path with
      | Error m -> Alcotest.fail m
      | Ok cfg ->
          Alcotest.(check string) "identity" "erde-gateway" cfg.Config.identity;
          Alcotest.(check int) "one provider" 1 (List.length cfg.Config.providers);
          let p = List.hd cfg.Config.providers in
          Alcotest.(check string) "wire" "openai"
            (match p.Config.wire with Config.Openai -> "openai" | _ -> "other");
          Alcotest.(check bool) "key resolves" true (Config.key cfg p = Ok "secret-value-here");
          let scrubbed = Config.redact cfg "token=secret-value-here" in
          Alcotest.(check bool) "redaction removes the value" false
            (contains scrubbed "secret-value-here"))

let test_config_missing_field_errors () =
  with_tmp (fun dir ->
      let path = Filename.concat dir "erde.json" in
      write path {|{"providers":[{"name":"x","wire":"openai"}]}|};
      match Config.load path with
      | Ok _ -> Alcotest.fail "a provider without base_url must be refused"
      | Error _ -> ())

(* ---------- turn (recorded response, no network) ---------- *)

let test_handle_response_journals_the_success_path () =
  with_tmp (fun dir ->
      let cfg_path = Filename.concat dir "erde.json" in
      write cfg_path config_json;
      write (Filename.concat dir "llm.env") "GOMODEL_API_KEY=k\n";
      Unix.chmod (Filename.concat dir "llm.env") 0o600;
      match Config.load cfg_path with
      | Error m -> Alcotest.fail m
      | Ok cfg ->
          let cfg = { cfg with Config.journal = Filename.concat dir "journal.jsonl" } in
          let p = List.hd cfg.Config.providers in
          let segs = big_segments () in
          let plan = Cache_strategy.plan p segs in
          (match
             Turn.handle_response ~cfg ~p ~plan ~model:"glm" ~task:"t" ~route:"cold" ~latency_ms:5
               ~status:200 ~raw:openai_json
           with
          | Error m -> Alcotest.fail m
          | Ok o ->
              Alcotest.(check int) "cached tokens in the outcome" 4096
                o.Turn.response.Wire.usage.Wire.cache_read);
          match Journal.read cfg.Config.journal with
          | Error m -> Alcotest.fail m
          | Ok rows ->
              Alcotest.(check int) "one row" 1 (List.length rows);
              let r = List.hd rows in
              Alcotest.(check string) "provider recorded" "gomodel" r.Journal.provider;
              Alcotest.(check string) "prefix hash recorded" plan.Cache_strategy.head
                r.Journal.prefix_hash;
              Alcotest.(check int) "cached tokens recorded" 4096 r.Journal.usage.Wire.cache_read;
              Alcotest.(check bool) "cost recorded" true (r.Journal.cost_usd > 0.0))

let test_handle_response_journals_a_failure () =
  with_tmp (fun dir ->
      let cfg_path = Filename.concat dir "erde.json" in
      write cfg_path config_json;
      write (Filename.concat dir "llm.env") "GOMODEL_API_KEY=k\n";
      Unix.chmod (Filename.concat dir "llm.env") 0o600;
      match Config.load cfg_path with
      | Error m -> Alcotest.fail m
      | Ok cfg ->
          let cfg = { cfg with Config.journal = Filename.concat dir "journal.jsonl" } in
          let p = List.hd cfg.Config.providers in
          let plan = Cache_strategy.plan p (big_segments ()) in
          (match
             Turn.handle_response ~cfg ~p ~plan ~model:"glm" ~task:"t" ~route:"cold" ~latency_ms:5
               ~status:401 ~raw:{|{"error":{"type":"authentication_error"}}|}
           with
          | Ok _ -> Alcotest.fail "401 must not be a success"
          | Error m -> Alcotest.(check bool) "error mentions status" true (String.contains m '4'));
          match Journal.read cfg.Config.journal with
          | Error m -> Alcotest.fail m
          | Ok rows ->
              Alcotest.(check int) "the attempt is still a row" 1 (List.length rows);
              Alcotest.(check string) "outcome is the status" "http 401"
                (List.hd rows).Journal.outcome)

let () =
  Alcotest.run "erde"
    [
      ( "segments",
        [
          Alcotest.test_case "order is enforced" `Quick test_order_enforced;
          Alcotest.test_case "claim order is canonical" `Quick test_hash_stable_under_claim_reorder;
          Alcotest.test_case "an early edit moves later boundaries" `Quick
            test_early_edit_moves_later_boundaries;
        ] );
      ( "cache",
        [
          Alcotest.test_case "implicit provider places nothing" `Quick
            test_implicit_has_no_breakpoints;
          Alcotest.test_case "below the minimum, a write is a loss" `Quick
            test_explicit_below_minimum_places_none;
          Alcotest.test_case "explicit breakpoints at shared boundaries" `Quick
            test_explicit_above_minimum_breaks_at_shared_boundaries;
        ] );
      ( "wire",
        [
          Alcotest.test_case "anthropic usage and cost" `Quick test_anthropic_usage_and_cost;
          Alcotest.test_case "openai usage and cost" `Quick test_openai_usage_and_cost;
          Alcotest.test_case "anthropic request marks breakpoints" `Quick
            test_anthropic_request_places_breakpoints;
        ] );
      ( "route",
        [
          Alcotest.test_case "a retired candidate is dropped" `Quick test_retired_candidate_is_dropped;
          Alcotest.test_case "a warm provider keeps the prefix" `Quick test_warm_provider_wins;
          Alcotest.test_case "unknown model errors" `Quick test_unknown_model_errors;
        ] );
      ( "journal",
        [ Alcotest.test_case "round trip and hit rates" `Quick test_journal_round_trip ] );
      ("sse", [ Alcotest.test_case "parse" `Quick test_sse_parse ]);
      ( "config",
        [
          Alcotest.test_case "env parsing" `Quick test_env_parse;
          Alcotest.test_case "secrets file mode" `Quick test_env_file_mode_is_enforced;
          Alcotest.test_case "load" `Quick test_config_load;
          Alcotest.test_case "missing field" `Quick test_config_missing_field_errors;
        ] );
      ( "turn",
        [
          Alcotest.test_case "success path journals" `Quick
            test_handle_response_journals_the_success_path;
          Alcotest.test_case "failure path journals" `Quick
            test_handle_response_journals_a_failure;
        ] );
    ]
