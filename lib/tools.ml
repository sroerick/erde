(* Erde.Tools -- the tool host.

   Each tool is a typed call against the authority.  The grant check happens
   here, before anything leaves the process: a refused call is a journal row,
   not a provider round trip.  Arguments are typed JSON; nothing in this file
   builds a shell string or hands model text to a shell. *)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

type call = { prim : string; args : Yojson.Safe.t }

let arg_path args = match Config.field "path" args with Some (`String p) -> Some p | _ -> None

let path_required prim args =
  match arg_path args with
  | Some p -> Ok p
  | None -> Error (Printf.sprintf "%s: missing \"path\"" prim)

let gate grants ~prim ~path =
  match Grants.check grants ~prim ~path with
  | Ok row -> Ok row
  | Error m -> Error ("denied: " ^ m)
(* ---------- file tools: local hands, grant-gated by path ----------

   Same discipline as the tree ops: the grant is checked before the file is
touched, a refusal is an Error the caller journals, and nothing here builds
a shell string. Paths are canonicalised first, so "../" cannot walk a call
outside a granted prefix. Writes are capped so one turn cannot paste a
megabyte into the transcript. *)

let max_file_bytes = 1_048_576

let canon_read p =
  try Ok (Unix.realpath p)
  with Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "file: %s: %s" p (Unix.error_message e))

(* A write target need not exist yet: canonicalise the parent, keep the name. *)
let canon_write p =
  let parent = Filename.dirname p in
  let name = Filename.basename p in
  try Ok (Filename.concat (Unix.realpath parent) name)
  with Unix.Unix_error (e, _, _) ->
    Error (Printf.sprintf "file: %s: %s" parent (Unix.error_message e))

let read_file p =
  try
    let ic = open_in_bin p in
    let n = in_channel_length ic in
    if n > max_file_bytes then (
      close_in ic;
      Error (Printf.sprintf "file.read: %s is %d bytes (cap %d)" p n max_file_bytes))
    else
      let s = really_input_string ic n in
      close_in ic;
      Ok s
  with Sys_error m -> Error ("file.read: " ^ m)

let write_file p s =
  if String.length s > max_file_bytes then
    Error (Printf.sprintf "file.write: %d bytes (cap %d)" (String.length s) max_file_bytes)
  else
    try
      let oc = open_out_bin p in
      output_string oc s;
      close_out oc;
      Ok ()
    with Sys_error m -> Error ("file.write: " ^ m)

let count_literal hay needle =
  let nl = String.length needle in
  let hl = String.length hay in
  if nl = 0 then 0
  else
    let rec go i acc =
      if i + nl > hl then acc
      else if String.sub hay i nl = needle then go (i + nl) (acc + 1)
      else go (i + 1) acc
    in
    go 0 0

let index_of hay needle =
  let nl = String.length needle in
  let rec go i =
    if i + nl > String.length hay then None
    else if String.sub hay i nl = needle then Some i
    else go (i + 1)
  in
  go 0

let arg_string prim name args =
  match Config.field name args with
  | Some (`String s) -> Ok s
  | _ -> Error (Printf.sprintf "%s: missing \"%s\"" prim name)

(* ---------- shell.exec: one program, no shell ----------

   argv is exec'd verbatim; no shell ever parses it. cwd is canonicalised and
grant-checked like any other path, argv[0] must be absolute and listed in the
grant's cmd_allow, and the grant's env is laid over the inherited one (the
opam switch needs PATH and OCAMLLIB to find ocamlc). Each stream is capped so
a noisy build cannot flood the transcript. *)

let max_exec_bytes = 65_536
let max_exec_timeout = 600

let read_capped_file p =
  try
    let ic = open_in_bin p in
    let n = min (in_channel_length ic) max_exec_bytes in
    let s = really_input_string ic n in
    close_in ic;
    s
  with Sys_error _ -> ""

let env_put env k v =
  let pref = k ^ "=" in
  let keep e = not (Grants.starts_with ~prefix:pref e) in
  (pref ^ v) :: List.filter keep env

let shell_exec ~argv ~cwd ~timeout_s ~extra_env =
  let out = Filename.temp_file "erde-exec-out" ".log" in
  let err = Filename.temp_file "erde-exec-err" ".log" in
  let child_env =
    Array.of_list
      (List.fold_left
         (fun acc (k, v) -> env_put acc k v)
         (Array.to_list (Unix.environment ())) extra_env)
  in
  let fd_out = Unix.openfile out [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let fd_err = Unix.openfile err [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let started = Unix.gettimeofday () in
  let pid =
    match Unix.fork () with
    | 0 ->
        (try Unix.chdir cwd with _ -> ());
        (try Unix.dup2 fd_out Unix.stdout with _ -> ());
        (try Unix.dup2 fd_err Unix.stderr with _ -> ());
        (try Unix.execve argv.(0) argv child_env with _ -> ());
        exit 127
    | pid -> pid
  in
  Unix.close fd_out;
  Unix.close fd_err;
  let deadline = started +. float_of_int timeout_s in
  let poll () = match Unix.waitpid [ Unix.WNOHANG ] pid with 0, _ -> None | _, st -> Some st in
  let rec loop () =
    match poll () with
    | Some st -> (Some st, false)
    | None ->
        if Unix.gettimeofday () >= deadline then (
          (try Unix.kill pid Sys.sigterm with _ -> ());
          Unix.sleepf 0.2;
          match poll () with
          | Some st -> (Some st, true)
          | None ->
              (try Unix.kill pid Sys.sigkill with _ -> ());
              ((try Some (snd (Unix.waitpid [] pid)) with _ -> None), true))
        else (
          Unix.sleepf 0.05;
          loop ())
  in
  let status, timed_out = loop () in
  let stdout = read_capped_file out and stderr = read_capped_file err in
  (try Sys.remove out with _ -> ());
  (try Sys.remove err with _ -> ());
  let code =
    match status with
    | Some (Unix.WEXITED n) -> n
    | Some (Unix.WSIGNALED n) -> 128 + n
    | Some (Unix.WSTOPPED n) -> 128 + n
    | None -> -1
  in
  let ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.) in
  (code, stdout, stderr, timed_out, ms)
let dispatch ~env ~sw ~authority ~grants (c : call) =
  match c.prim with
  | "tree.get" -> (
      let* path = path_required "tree.get" c.args in
      let* _allowed = gate grants ~prim:"tree.get" ~path in
      let* value, hash, version = Authority.tree_get ~env ~sw authority ~path in
      Ok
        (`Assoc
          [
            ("path", `String path);
            ("value_ternary", `String value);
            ("value_hash", `String hash);
            ("version", `Int version);
          ]))
  | "tree.list" -> (
      let* prefix =
        match Config.field "prefix" c.args with
        | Some (`String p) -> Ok p
        | _ -> Error "tree.list: missing \"prefix\""
      in
      let* _allowed = gate grants ~prim:"tree.list" ~path:prefix in
      let* entries = Authority.tree_list ~env ~sw authority ~prefix ~limit:1000 in
      Ok
        (`Assoc
          [
            ( "entries",
              `List
                (List.map
                   (fun (e : Authority.entry) ->
                     `Assoc
                       [
                         ("path", `String e.Authority.path);
                         ("value_hash", `String e.Authority.value_hash);
                       ])
                   entries) );
          ]))
  | "tree.put" -> (
      let* path = path_required "tree.put" c.args in
      let* _allowed = gate grants ~prim:"tree.put" ~path in
      let value =
        match Config.field "value_ternary" c.args with
        | Some (`String value) -> Some value
        | _ -> (
            match Config.field "value_text" c.args with
            | Some (`String text) -> Some (Treecalc.to_ternary (Treecalc.chunked_text text))
            | _ -> None)
      in
      match value with
      | Some value ->
          let* hash, version =
            Authority.tree_put ~env ~sw authority ~path ~value_ternary:value
          in
          Ok
            (`Assoc
              [
                ("path", `String path);
                ("value_hash", `String hash);
                ("version", `Int version);
              ])
      | None -> Error "tree.put: missing \"value_ternary\" or \"value_text\"")
  | "file.read" -> (
      let* path = path_required "file.read" c.args in
      let* real = canon_read path in
      let* _allowed = gate grants ~prim:"file.read" ~path:real in
      let* content = read_file real in
      Ok
        (`Assoc
          [
            ("path", `String real);
            ("bytes", `Int (String.length content));
            ("content", `String content);
          ]))
  | "file.write" -> (
      let* path = path_required "file.write" c.args in
      let* content = arg_string "file.write" "content" c.args in
      let* real = canon_write path in
      let* _allowed = gate grants ~prim:"file.write" ~path:real in
      let* () = write_file real content in
      Ok (`Assoc [ ("path", `String real); ("bytes", `Int (String.length content)) ]))
  | "file.patch" -> (
      (* a patch is a write: it needs the file.write grant on the target path *)
      let* path = path_required "file.patch" c.args in
      let* old_s = arg_string "file.patch" "old" c.args in
      let* new_s = arg_string "file.patch" "new" c.args in
      let* real = canon_read path in
      let* _allowed = gate grants ~prim:"file.write" ~path:real in
      let* hay = read_file real in
      let n = count_literal hay old_s in
      if n <> 1 then
        Error
          (Printf.sprintf "file.patch: the old literal occurs %d times (exactly 1 required)" n)
      else
        match index_of hay old_s with
        | None -> Error "file.patch: literal vanished between count and replace"
        | Some i ->
            let ol = String.length old_s in
            let out =
              String.sub hay 0 i ^ new_s
              ^ String.sub hay (i + ol) (String.length hay - i - ol)
            in
            let* () = write_file real out in
            Ok (`Assoc [ ("path", `String real); ("replaced", `Int 1) ]))
  | "file.list" -> (
      let* path = path_required "file.list" c.args in
      let* real = canon_read path in
      (* a directory is granted as a prefix, so check it with the trailing
         slash the grant table uses: "/x/wt/" covers listing "/x/wt" *)
      let* _allowed = gate grants ~prim:"file.list" ~path:(real ^ "/") in
        match
          try Ok (Array.to_list (Sys.readdir real)) with
          | Sys_error m -> Error ("file.list: " ^ m)
        with
        | Error m -> Error m
        | Ok names ->
            Ok
              (`Assoc
                [
                  ("path", `String real);
                  ("entries", `List (List.map (fun s -> `String s) (List.sort compare names)));
                ]))
  | "shell.exec" -> (
      let* cwd = arg_string "shell.exec" "cwd" c.args in
      let* real = canon_read cwd in
      if not (Sys.file_exists real && Sys.is_directory real) then
        Error ("shell.exec: " ^ real ^ " is not a directory")
      else
        let* argv =
          match Config.field "argv" c.args with
          | Some (`List xs) -> (
              match List.filter_map (function `String s -> Some s | _ -> None) xs with
              | [] -> Error "shell.exec: \"argv\" must be a non-empty array of strings"
              | parts -> Ok (Array.of_list parts))
          | _ -> Error "shell.exec: missing \"argv\" (array of strings)"
        in
        let timeout_s =
          match Config.field "timeout_s" c.args with
          | Some (`Int n) -> max 1 (min n max_exec_timeout)
          | _ -> 60
        in
          (* grants name directories with a trailing slash; the canonical
             cwd has none, so check it the way file.list does *)
          let* allowed = gate grants ~prim:"shell.exec" ~path:(real ^ "/") in
        let prog = argv.(0) in
        if Filename.is_relative prog then
          Error ("shell.exec: argv[0] must be an absolute path, got " ^ prog)
        else if not (List.mem prog allowed.Grants.cmd_allow) then
          Error
            (Printf.sprintf "shell.exec: %s is not in this grant's cmd_allow (%s)" prog
               (String.concat "," allowed.Grants.cmd_allow))
        else
          let code, stdout, stderr, timed_out, ms =
            shell_exec ~argv ~cwd:real ~timeout_s ~extra_env:allowed.Grants.env
          in
          Ok
            (`Assoc
              [
                ("cwd", `String real);
                ("exit", `Int code);
                ("timed_out", `Bool timed_out);
                ("ms", `Int ms);
                ("stdout", `String stdout);
                ("stderr", `String stderr);
              ]))
  | "echo" -> (
      (* the local sanity prim: no authority, but it still needs a grant, so
         the gate has one path in and one path out for every tool *)
      let* _allowed = gate grants ~prim:"echo" ~path:"" in
      Ok c.args)
  | "clock" -> (
      (* the harness clock: no authority, no path, but still grant-gated *)
      let* _allowed = gate grants ~prim:"clock" ~path:"" in
      let now = Unix.time () in
      let g = Unix.gmtime now in
      Ok
        (`Assoc
          [
            ("epoch", `Float now);
            ( "iso",
              `String
                (Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (g.Unix.tm_year + 1900)
                    (g.Unix.tm_mon + 1) g.Unix.tm_mday g.Unix.tm_hour g.Unix.tm_min
                    g.Unix.tm_sec));
          ]))
  | other -> Error (Printf.sprintf "unknown prim %s" other)


(* The schemas the model sees.  A tool the model cannot name is a tool the
   loop cannot reach, so these are part of the host, not of the caller. *)
let schema wire prim description properties required =
  match wire with
  | Config.Anthropic ->
      `Assoc
        [
          ("name", `String prim);
          ("description", `String description);
          ("input_schema", `Assoc [ ("type", `String "object"); ("properties", `Assoc properties); ("required", `List (List.map (fun r -> `String r) required)) ]);
        ]
  | Config.Openai ->
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String prim);
                ("description", `String description);
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ("properties", `Assoc properties);
                      ("required", `List (List.map (fun r -> `String r) required));
                    ] );
              ] );
        ]

let str_prop description = `Assoc [ ("type", `String "string"); ("description", `String description) ]

let schemas wire =
  [
    schema wire "file.read" "Read a UTF-8 text file. Grant-gated by path prefix; 1MB cap."
      [ ("path", str_prop "Absolute path") ] [ "path" ];
    schema wire "file.write"
      "Write a whole text file (creates or truncates; the parent directory must exist; 1MB cap). Needs the file.write grant."
      [ ("path", str_prop "Absolute path"); ("content", str_prop "Full file content") ]
      [ "path"; "content" ];
    schema wire "file.patch"
      "Replace exactly one literal occurrence in a text file; fails unless the old literal occurs exactly once. Needs the file.write grant."
      [
        ("path", str_prop "Absolute path");
        ("old", str_prop "Exact literal to replace");
        ("new", str_prop "Replacement literal");
      ]
      [ "path"; "old"; "new" ];
    schema wire "file.list" "List a directory's entries, sorted."
      [ ("path", str_prop "Absolute directory path") ] [ "path" ];
    schema wire "shell.exec"
      "Run one program with no shell: argv is exec'd verbatim in cwd with a timeout. argv[0] must be absolute and listed in the grant's cmd_allow; the grant supplies PATH and OCAMLLIB. Each stream is capped at 64KB."
      [
        ( "argv",
          `Assoc
            [
              ("type", `String "array");
              ("items", `Assoc [ ("type", `String "string") ]);
              ( "description",
                `String
                  "Program and arguments, e.g. [\"/home/al/.opam/poohstack/bin/dune\",\"build\"]" );
            ] );
        ("cwd", str_prop "Absolute working directory inside a granted prefix");
        ( "timeout_s",
          `Assoc
            [
              ("type", `String "integer");
              ("description", `String "Seconds before SIGTERM (default 60, max 600)");
            ] );
      ]
      [ "argv"; "cwd" ];
    schema wire "tree.get" "Read the canonical ternary value at a tree path."
      [ ("path", str_prop "Tree path, e.g. u.marin.notes") ] [ "path" ];
    schema wire "tree.list" "List the tree entries under a prefix."
      [ ("prefix", str_prop "Raw string prefix, e.g. u.marin.") ] [ "prefix" ];
    schema wire "tree.put" "Write a value at a path (text is stored chunked-claim encoded)."
      [
        ("path", str_prop "Tree path to write");
        ("value_ternary", str_prop "Canonical ternary: 0 leaf, 1+child stem, 2+left+right fork");
        ("value_text", str_prop "UTF-8 text; the harness chunk-encodes it (use this OR value_ternary)")
      ]
      [ "path" ];
    schema wire "clock" "Read the harness clock: epoch seconds and ISO-8601 UTC." [ ("format", str_prop "Optional. Only \"utc\" or \"epoch\" is accepted.") ] [];
  ]
