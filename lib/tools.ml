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
  | "echo" -> (
      (* the local sanity prim: no authority, but it still needs a grant, so
         the gate has one path in and one path out for every tool *)
      let* _allowed = gate grants ~prim:"echo" ~path:"" in
      Ok c.args)
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
  ]
