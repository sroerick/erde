(* Erde.Grants -- the gateway's own gate, checked before a tool runs.

   Two authorities, deliberately.  This table is what the gateway will do;
   tuna's tree-op floor is what happens if the gateway is wrong.  A call whose
   path no local grant covers is never sent, so the refusal costs nothing and
   shows up as a journal row rather than a provider round trip.

   Prefix matching is tuna's rule, verbatim: a raw string prefix over the
   dot-separated namespaces, so "u.marin." covers u.marin.notes and "u.marin/"
   covers nothing.  An empty prefix is refused rather than normalized,
   because it reads as scope while meaning everything. *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

type row = {
  id : string;
  prim : string;                 (* "tree.get", "tree.list", "tree.put", ... *)
  path_prefix : string option;   (* None = unscoped: covers every path *)
  cmd_allow : string list;       (* shell.exec: absolute argv[0] values this grant may run *)
  env : (string * string) list;  (* shell.exec: variables laid over the inherited env *)
  live : bool;
}

type t = row list

let starts_with ~prefix s =
  let pl = String.length prefix in
  String.length s >= pl && String.sub s 0 pl = prefix

let covers row path =
  match row.path_prefix with None -> true | Some "" -> false | Some p -> starts_with ~prefix:p path

let live_rows t = List.filter (fun r -> r.live) t

(* The gate.  Returns the row that allows the call, or why nothing does. *)
let check t ~prim ~path =
  let candidates = List.filter (fun r -> r.prim = prim) (live_rows t) in
  if candidates = [] then
    Error (Printf.sprintf "no live grant for prim %s" prim)
  else
    match List.find_opt (fun r -> covers r path) candidates with
    | Some r -> Ok r
    | None ->
        Error
          (Printf.sprintf "no grant for %s covers %s (granted prefixes: %s)" prim path
             (String.concat ","
                (List.map
                   (fun r -> match r.path_prefix with Some p -> p | None -> "*")
                   candidates)))

(* ---------- json ---------- *)

let field name = function `Assoc kvs -> List.assoc_opt name kvs | _ -> None

let str_opt name j = match field name j with Some (`String s) -> Some s | _ -> None
let str name j = match str_opt name j with Some s -> s | None -> ""
let bool_or name d j = match field name j with Some (`Bool b) -> b | _ -> d

let str_list name j =
  match field name j with
  | Some (`List xs) -> List.filter_map (function `String s -> Some s | _ -> None) xs
  | _ -> []

let str_assoc name j =
  match field name j with
  | Some (`Assoc kvs) ->
      List.filter_map (function k, `String v -> Some (k, v) | _ -> None) kvs
  | _ -> []

let of_json j : (row, string) result =
  match (str_opt "prim" j, field "id" j) with
  | Some prim, Some (`String id) ->
      Ok
        {
          id;
          prim;
          path_prefix = str_opt "path_prefix" j;
          cmd_allow = str_list "cmd_allow" j;
          env = str_assoc "env" j;
          live = bool_or "live" true j;
        }
  | _ -> Error "grant needs \"id\" and \"prim\""

(* GET /api/grants/:id answers {"grant": {...}, "lineage": [...], ...}. *)
let of_tuna_grant j =
  match field "grant" j with
  | Some g ->
      let* r = of_json g in
      (* a revoked grant row is a fact; it is simply not a live capability *)
      let revoked = match field "revoked_at" g with Some (`String _) -> true | _ -> false in
      Ok { r with live = r.live && not revoked }
  | None -> Error "not a tuna grant envelope"
