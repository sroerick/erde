open Erde


let test_classify () =
  let t st b = Alcotest.(check string) "kind" "transient"
      (match Retry.classify ~status:st ~body:b with Transient -> "transient" | Permanent -> "permanent") in
  let p st b = Alcotest.(check string) "kind" "permanent"
      (match Retry.classify ~status:st ~body:b with Transient -> "transient" | Permanent -> "permanent") in
  t 429 "{}";
  t 500 "{}";
  t 503 "upstream unavailable";
  t 408 "";
  t 400 "please try again later";
  p 400 "This Go model requires Global regions. Select Global in your workspace's Privacy settings to use it.";
  p 400 "invalid request: missing model";
  p 401 "unauthorized";
  p 403 "forbidden";
  p 404 "no such route";
  p 200 "{}"

let test_backoff () =
  Alcotest.(check int) "attempt 1" 2000 (Retry.backoff_ms ~base_ms:2000 ~attempt:1);
  Alcotest.(check int) "attempt 2" 4000 (Retry.backoff_ms ~base_ms:2000 ~attempt:2);
  Alcotest.(check int) "attempt 3" 8000 (Retry.backoff_ms ~base_ms:2000 ~attempt:3);
  Alcotest.(check int) "capped" 20000 (Retry.backoff_ms ~base_ms:2000 ~attempt:9)

let test_retries_transport_then_ok () =
  Eio_main.run @@ fun env ->
  let calls = ref 0 in
  let r =
    Retry.request ~attempts:3 ~base_ms:1 ~clock:(Eio.Stdenv.clock env) ~label:"t" (fun () ->
        incr calls;
        if !calls < 3 then Error "connection reset" else Ok (200, "{}"))
  in
  Alcotest.(check int) "three attempts" 3 !calls;
  (match r with
  | Ok (200, _) -> ()
  | _ -> Alcotest.fail "expected the third attempt to succeed")

let test_stops_on_permanent () =
  Eio_main.run @@ fun env ->
  let calls = ref 0 in
  let r =
    Retry.request ~attempts:5 ~base_ms:1 ~clock:(Eio.Stdenv.clock env) ~label:"t" (fun () ->
        incr calls;
        Ok (400, "This Go model requires Global regions."))
  in
  Alcotest.(check int) "one attempt only" 1 !calls;
  match r with Ok (400, _) -> () | _ -> Alcotest.fail "expected the 400 to pass through"

let test_exhausts_and_reports () =
  Eio_main.run @@ fun env ->
  let calls = ref 0 in
  let r =
    Retry.request ~attempts:2 ~base_ms:1 ~clock:(Eio.Stdenv.clock env) ~label:"t" (fun () ->
        incr calls;
        Error "dns failure")
  in
  Alcotest.(check int) "two attempts" 2 !calls;
  match r with
  | Error m -> Alcotest.(check bool) "says attempts" true (String.length m > 0)
  | Ok _ -> Alcotest.fail "expected an error after exhausting attempts"

let () =
  Alcotest.run "erde-retry"
    [
      ("classify", [ Alcotest.test_case "statuses and bodies" `Quick test_classify ]);
      ("backoff", [ Alcotest.test_case "exponential, capped" `Quick test_backoff ]);
      ( "loop",
        [
          Alcotest.test_case "transport retried then ok" `Quick test_retries_transport_then_ok;
          Alcotest.test_case "permanent stops at one" `Quick test_stops_on_permanent;
          Alcotest.test_case "exhaustion reports" `Quick test_exhausts_and_reports;
        ] );
    ]
