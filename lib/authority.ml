(* Erde.Authority -- the tuna client, and the only place a bearer lives.

   The store is HTTP-only (the design's law): no linked library, no ambient
   access, every read and write crossing the grant boundary.  The token never
   reaches a prompt, a journal row, or a log line; [scrub] removes it from any
   message on the way back to a caller, the same defence-in-depth as
   Config.redact.

   Claims are tree paths.  Reading an identity's claims is a prefix list plus
   a get per entry; the value's citation hash is the claim's source, and the
   canonical ternary string is its value. *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

type t = { base_url : string; token : string }

let make ~base_url ~token = { base_url; token }

let scrub t s =
  if String.length t.token >= 8 then Config.replace_all ~needle:t.token ~by:"[redacted]" s else s

let truncate n s = if String.length s <= n then s else String.sub s 0 n ^ "..."

let headers t =
  [ ("authorization", "Bearer " ^ t.token); ("content-type", "application/json") ]

let call ~env ~sw t ~meth ~path ?body () =
  let url = t.base_url ^ path in
  match Http_client.request ~env ~sw ~meth ~url ~headers:(headers t) ?body () with
  | Error m -> Error (scrub t m)
  | Ok (status, body) ->
      if status >= 200 && status < 300 then Ok body
      else
        Error
          (Printf.sprintf "tuna %s %s: http %d: %s" meth path status
             (scrub t (truncate 300 body)))

let post ~env ~sw t path json =
  call ~env ~sw t ~meth:"POST" ~path ~body:(Yojson.Safe.to_string json) ()

let json_of ~scrub_with body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error m -> Error (scrub_with m)
  | j -> Ok j

(* ---------- reads ---------- *)

type entry = { path : string; value_hash : string; version : int }

let entry_of = function
  | `Assoc _ as j ->
      Some
        {
          path = Config.str_or "path" "" j;
          value_hash = Config.str_or "value_hash" "" j;
          version = int_of_float (Config.num_or "version" 0.0 j);
        }
  | _ -> None

let tree_list ~env ~sw t ~prefix ~limit =
  let* body =
    post ~env ~sw t "/api/tree/list"
      (`Assoc [ ("prefix", `String prefix); ("limit", `Int limit) ])
  in
  let* j = json_of ~scrub_with:(scrub t) body in
  match Config.field "entries" j with
  | Some (`List l) -> Ok (List.filter_map entry_of l)
  | Some `Null -> Ok []
  | _ -> Error "tuna tree/list: no entries array"

let tree_get ~env ~sw t ~path =
  let* body = post ~env ~sw t "/api/tree/get" (`Assoc [ ("path", `String path) ]) in
  let* j = json_of ~scrub_with:(scrub t) body in
  match Config.field "value_ternary" j with
  | Some (`String v) ->
      Ok (v, Config.str_or "value_hash" "" j, int_of_float (Config.num_or "version" 0.0 j))
  | _ -> Error (Printf.sprintf "tuna tree/get: no value at %s" path)

let whoami ~env ~sw t =
  let* body = call ~env ~sw t ~meth:"GET" ~path:"/api/whoami" () in
  let* j = json_of ~scrub_with:(scrub t) body in
  Ok (Config.str_or "name" "" j, Config.str_or "identity" "" j)

(* ---------- writes ---------- *)

let tree_put ~env ~sw t ~path ~value_ternary =
  let* body =
    post ~env ~sw t "/api/tree/put"
      (`Assoc [ ("path", `String path); ("value_ternary", `String value_ternary) ])
  in
  let* j = json_of ~scrub_with:(scrub t) body in
  Ok (Config.str_or "value_hash" "" j, int_of_float (Config.num_or "version" 0.0 j))

(* ---------- claims ---------- *)

(* The identity's claims as segments input: path, the canonical ternary value,
   and the citation hash that makes the claim verifiable. *)
(* One get per entry, so the caller caps the fan-out: a big garden is a slow
   read, and the first slice would rather state its coverage than stall on it. *)
let of_entries ~env ~sw t entries =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | (e : entry) :: rest -> (
        match tree_get ~env ~sw t ~path:e.path with
        | Error m -> Error m
        | Ok (value, hash, _) ->
            let source = if hash = "" then e.value_hash else hash in
            go ({ Segments.Claim_set.path = e.path; value; source } :: acc) rest)
  in
  go [] entries

let claims ~env ~sw t ~prefix ~limit =
  let* entries = tree_list ~env ~sw t ~prefix ~limit in
  of_entries ~env ~sw t entries

(* [cap] is the number of values to fetch; [total] is what the prefix holds,
   so a caller can say how much of the garden it actually read. *)
let claims_capped ~env ~sw t ~prefix ~cap =
  let* entries = tree_list ~env ~sw t ~prefix ~limit:1000 in
  let taken = List.filteri (fun i _ -> i < cap) entries in
  let* cs = of_entries ~env ~sw t taken in
  Ok (cs, List.length entries)
