(* Erde.Config -- the gateway's instance configuration.

   Two files, deliberately separate:

     erde.json   config, no secrets: providers, prices, cache modes, routes
     llm.env     secrets, mode 600: one key per provider, referenced by name

   A provider row names the env var holding its key.  The key value never
   enters this type, the journal, or a log line; [redact] scrubs loaded
   values out of any string on its way back to a caller. *)

type wire = Anthropic | Openai
type cache_mode = Explicit | Implicit

type prices = {
  input : float;
  output : float;
  cache_write_mult : float;
  cache_read_mult : float;
}

type cache = { mode : cache_mode; ttl_s : int; min_prefix_tokens : int }

type provider = {
  name : string;
  wire : wire;
  base_url : string;
  key_env : string;
  auth_header : string;
  auth_scheme : string;  (* "Bearer " or "" for x-api-key style *)
  prices : prices;
  cache : cache;
  live : bool;
}

type route = { model : string; candidates : string list }

(* The authority is tuna.  Base URL and the env var holding the bearer; the
   token itself never appears in this type. *)
type authority = { base_url : string; token_env : string; claims_prefix : string }

type t = {
  path : string;
  journal : string;
  identity : string;
  providers : provider list;
  routes : route list;
  authority : authority;
  grants : Grants.row list;
  env : (string * string) list;
}

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

(* ---------- env files ---------- *)

let parse_env text =
  let unquote s =
    let n = String.length s in
    if n >= 2
       && ((s.[0] = '"' && s.[n - 1] = '"') || (s.[0] = '\'' && s.[n - 1] = '\''))
    then String.sub s 1 (n - 2)
    else s
  in
  let strip_export line =
    let p = "export " in
    if String.length line > String.length p && String.sub line 0 (String.length p) = p
    then String.trim (String.sub line (String.length p) (String.length line - String.length p))
    else line
  in
  String.split_on_char '\n' text
  |> List.filter_map (fun raw ->
         let line = String.trim raw in
         if line = "" || line.[0] = '#' then None
         else
           let line = strip_export line in
           match String.index_opt line '=' with
           | None -> None
           | Some i ->
               let k = String.trim (String.sub line 0 i) in
               let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
               if k = "" then None else Some (k, unquote v))

let read_file path =
  try
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Ok s
  with Sys_error m -> Error m

let load_env_file ?(allow_insecure = false) path =
  let* () =
    match Unix.stat path with
    | exception Unix.Unix_error (e, _, _) ->
        errf "cannot stat env file %s: %s" path (Unix.error_message e)
    | st ->
        let perm = st.Unix.st_perm in
        if perm land 0o077 <> 0 && not allow_insecure then
          errf
            "env file %s is mode %o: secrets must be mode 600 (set \
             ERDE_ALLOW_INSECURE_ENV=1 to override)"
            path perm
        else Ok ()
  in
  let* text =
    match read_file path with Ok t -> Ok t | Error m -> errf "cannot read %s: %s" path m
  in
  Ok (parse_env text)

(* ---------- json helpers ---------- *)

let field name = function `Assoc kvs -> List.assoc_opt name kvs | _ -> None

let req name j =
  match field name j with Some v -> Ok v | None -> errf "config: missing field %s" name

let str name j =
  let* v = req name j in
  match v with `String s -> Ok s | _ -> errf "config: %s must be a string" name

let num name j =
  let* v = req name j in
  match v with
  | `Float f -> Ok f
  | `Int i -> Ok (float_of_int i)
  | _ -> errf "config: %s must be a number" name

let num_or name d j =
  match field name j with
  | Some (`Float f) -> f
  | Some (`Int i) -> float_of_int i
  | _ -> d

let bool_or name d j = match field name j with Some (`Bool b) -> b | _ -> d

let str_or name d j = match field name j with Some (`String s) -> s | _ -> d

let list_of name j = match field name j with Some (`List l) -> l | _ -> []

let strings name j = list_of name j |> List.filter_map (function `String s -> Some s | _ -> None)

let uniq_sorted xs = List.sort_uniq compare xs

(* ---------- parsing ---------- *)

let parse_wire = function
  | "anthropic" -> Ok Anthropic
  | "openai" -> Ok Openai
  | s -> errf "config: unknown wire %S (anthropic|openai)" s

let parse_cache_mode = function
  | "explicit" -> Ok Explicit
  | "implicit" -> Ok Implicit
  | s -> errf "config: unknown cache mode %S (explicit|implicit)" s

let parse_provider j =
  let* name = str "name" j in
  let* wire_s = str "wire" j in
  let* wire = parse_wire wire_s in
  let* base_url = str "base_url" j in
  let* key_env = str "key_env" j in
  let* cache_s = str_or "cache_mode" "explicit" j |> fun s -> parse_cache_mode s in
  let prices =
    {
      input = num_or "input_price" 0.0 j;
      output = num_or "output_price" 0.0 j;
      cache_write_mult = num_or "cache_write_mult" 1.25 j;
      cache_read_mult = num_or "cache_read_mult" 0.1 j;
    }
  in
  let cache =
    {
      mode = cache_s;
      ttl_s = int_of_float (num_or "cache_ttl_s" 300.0 j);
      min_prefix_tokens = int_of_float (num_or "min_prefix_tokens" 1024.0 j);
    }
  in
  Ok
    {
      name;
      wire;
      base_url;
      key_env;
      auth_header = str_or "auth_header" "Authorization" j;
      auth_scheme = str_or "auth_scheme" "Bearer " j;
      prices;
      cache;
      live = bool_or "live" true j;
    }

let parse_route j =
  let* model = str "model" j in
  Ok { model; candidates = strings "candidates" j }

let load ?env_file ?(allow_insecure = false) path =
  let* text =
    match read_file path with Ok t -> Ok t | Error m -> errf "cannot read config %s: %s" path m
  in
  let* json =
    try Ok (Yojson.Safe.from_string text)
    with Yojson.Json_error m -> errf "config %s: %s" path m
  in
  let* providers =
    match field "providers" json with
    | Some (`List l) ->
        List.fold_left
          (fun acc j ->
            let* acc = acc in
            let* p = parse_provider j in
            Ok (p :: acc))
          (Ok []) l
        |> fun r -> (match r with Ok l -> Ok (List.rev l) | Error _ as e -> e)
    | _ -> errf "config %s: providers must be a list" path
  in
  let routes = list_of "routes" json |> List.filter_map (fun j -> match parse_route j with Ok r -> Some r | Error _ -> None) in
  let dir = Filename.dirname path in
  let env_file = match env_file with Some f -> f | None -> Filename.concat dir "llm.env" in
  let* env =
    if Sys.file_exists env_file then load_env_file ~allow_insecure env_file
    else Ok []
  in
  let authority =
    let a = match field "authority" json with Some j -> j | None -> `Assoc [] in
    {
      base_url = str_or "base_url" "http://127.0.0.1:18090" a;
      token_env = str_or "token_env" "TUNA_TOKEN" a;
      claims_prefix = str_or "claims_prefix" "" a;
    }
  in
  let grants =
    list_of "grants" json
    |> List.filter_map (fun j -> match Grants.of_json j with Ok r -> Some r | Error _ -> None)
  in
  Ok
    {
      path;
      journal = str_or "journal" (Filename.concat dir "journal.jsonl") json;
      identity = str_or "identity" "unattributed" json;
      providers;
      routes;
      authority;
      grants;
      env;
    }

let provider t name = List.find_opt (fun p -> p.name = name) t.providers
let all_providers t = t.providers

(* Routing is a cost decision, not just an availability decision: a fallover
   switches prefixes, and a switched prefix destroys every warm cache entry at
   exactly the moment a provider is degraded.  So a candidate list is ordered
   by cache affinity first, then cost; a provider retired for the lane is
   simply absent, and the reason string says so out loud. *)
let live_candidates t route =
  List.filter_map
    (fun name ->
      match provider t name with Some p when p.live -> Some p | _ -> None)
    route.candidates

let choose t ?warm ~model () =
  match List.find_opt (fun r -> r.model = model) t.routes with
  | None -> errf "no route for model %s" model
  | Some route -> (
      let cands = live_candidates t route in
      match cands with
      | [] -> errf "no live candidate for model %s (candidates: %s)" model (String.concat "," route.candidates)
      | first :: _ ->
          let live_names = List.map (fun p -> p.name) cands in
          let dropped = List.filter (fun n -> not (List.mem n live_names)) route.candidates in
          let note =
            if dropped = [] then ""
            else Printf.sprintf " (retired for this lane: %s)" (String.concat "," dropped)
          in
          (match warm with
          | Some w when List.mem w live_names ->
              let p = List.find (fun p -> p.name = w) cands in
              Ok (p, Printf.sprintf "warm: %s already holds this prefix%s" w note)
          | _ -> Ok (first, Printf.sprintf "cold: first live candidate for %s%s" model note)))

(* ---------- secret handling ---------- *)

let key t p =
  let from_env k = match List.assoc_opt k t.env with Some v when v <> "" -> Some v | _ -> None in
  match from_env p.key_env with
  | Some v -> Ok v
  | None -> (
      match Sys.getenv_opt p.key_env with
      | Some v when v <> "" -> Ok v
      | _ -> errf "provider %s: no secret in %s" p.name p.key_env)

let authority_token t =
  let from_env k = match List.assoc_opt k t.env with Some v when v <> "" -> Some v | _ -> None in
  match from_env t.authority.token_env with
  | Some v -> Ok v
  | None -> (
      match Sys.getenv_opt t.authority.token_env with
      | Some v when v <> "" -> Ok v
      | _ -> errf "authority: no bearer in %s" t.authority.token_env)

let replace_all ~needle ~by hay =
  if needle = "" then hay
  else begin
    let nl = String.length needle and hl = String.length hay in
    let buf = Buffer.create hl in
    let rec go i =
      if i >= hl then ()
      else if i + nl <= hl && String.sub hay i nl = needle then begin
        Buffer.add_string buf by;
        go (i + nl)
      end
      else begin
        Buffer.add_char buf hay.[i];
        go (i + 1)
      end
    in
    go 0;
    Buffer.contents buf
  end

(* Scrub every loaded secret out of a string before it reaches a caller, a
   log line, or the journal.  Defence in depth: the ordinary path never puts
   a key in a string in the first place. *)
let redact t s =
  List.fold_left
    (fun acc (_, v) -> if String.length v >= 8 then replace_all ~needle:v ~by:"[redacted]" acc else acc)
    s t.env
