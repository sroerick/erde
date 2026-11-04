(* Erde.Retry -- bounded retry around the provider call.

   Http_client.request reports failure in two shapes: the Error arm (DNS,
   connect, TLS, reset, timeout) and a non-2xx status carrying a body.

   Transport errors are worth another attempt. A status is worth another
   attempt only when it names a transient condition (429, 5xx, or a 4xx whose
   body says "try again"/"overloaded"/"rate limit"). A provider gate such as
   "this Go model requires Global regions" arrives as 400 and will say exactly
   the same thing on every attempt: retrying it spends the call twice and
   delays the log line that tells a human what to change. That case stops at
   the first response and says so.

   Safe because the call is buffered: Http_client.request returns the whole
   body, so a retry cannot duplicate output that a caller already consumed. *)

let contains hay needle =
  let hl = String.length hay and nl = String.length needle in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let clip n s = if String.length s <= n then s else String.sub s 0 n ^ "..."

type kind = Transient | Permanent

let transient_status = function
  | 408 | 425 | 429 -> true
  | n -> n >= 500

(* A 4xx that names a transient condition is still transient. *)
let transient_body body =
  let b = String.lowercase_ascii body in
  List.exists (contains b)
    [ "rate limit"; "overloaded"; "try again"; "temporarily"; "capacity"; "timeout" ]

let classify ~status ~body : kind =
  if transient_status status then Transient
  else if status >= 400 && status < 500 && transient_body body then Transient
  else Permanent

let env_int name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some s -> (
      match int_of_string_opt (String.trim s) with Some n when n > 0 -> n | _ -> default)

let attempts_default () = env_int "ERDE_RETRY_ATTEMPTS" 3
let base_ms_default () = env_int "ERDE_RETRY_BASE_MS" 2000
let cap_ms = 20_000

let backoff_ms ~base_ms ~attempt = min cap_ms (base_ms * (1 lsl (max 0 (attempt - 1))))

(** [request ~clock ~label f] runs [f] up to [attempts] times while the result
    is a retryable failure, and returns the LAST result unchanged so the
    caller's existing handling (journal row, error text) still runs. *)
let request ?(attempts = attempts_default ()) ?(base_ms = base_ms_default ()) ~clock
    ~label (f : unit -> (int * string, string) result) : (int * string, string) result =
  let rec go n =
    match f () with
    | Ok (status, body) -> (
        match classify ~status ~body with
        | Transient when n < attempts ->
            let ms = backoff_ms ~base_ms ~attempt:n in
            Printf.eprintf "erde: %s -> HTTP %d, retry %d/%d in %dms\n%!" label status n
              (attempts - 1) ms;
            Eio.Time.sleep clock (float_of_int ms /. 1000.0);
            go (n + 1)
        | _ -> Ok (status, body))
    | Error e ->
        if n < attempts then begin
          let ms = backoff_ms ~base_ms ~attempt:n in
          Printf.eprintf "erde: %s -> %s, retry %d/%d in %dms\n%!" label (clip 200 e) n
            (attempts - 1) ms;
          Eio.Time.sleep clock (float_of_int ms /. 1000.0);
          go (n + 1)
        end
        else Error (Printf.sprintf "%s (after %d attempt(s))" e n)
  in
  go 1
