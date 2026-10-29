(* Erde.Wire -- request building and response parsing for the two wire
   shapes, plus the usage accounting that differs between them.

   Two asymmetries matter and both are load-bearing:

     1. Anthropic reports input_tokens EXCLUDING cache reads and writes, and
        names the cache counters separately.  OpenAI reports prompt_tokens
        INCLUDING cached tokens, with cached_tokens as a subset.
     2. Anthropic charges the cache write (1.25x / 2x).  OpenAI's write is
        not an additive fee.

   Cost per call is therefore per-wire, not shared arithmetic on a common
   usage record. *)

type usage = {
  input : int;         (* billable non-cached input tokens *)
  output : int;
  cache_write : int;
  cache_read : int;
}

let usage_zero = { input = 0; output = 0; cache_write = 0; cache_read = 0 }

type tool_call = { name : string; args : Yojson.Safe.t }
type response = { text : string option; tool_calls : tool_call list; usage : usage; stop : string option }

let int_of = function `Int i -> i | `Float f -> int_of_float f | _ -> 0

let field name = function `Assoc kvs -> List.assoc_opt name kvs | _ -> None

let opt_string_field name j = match field name j with Some (`String s) -> Some s | _ -> None

let opt_int j path =
  let rec go j = function
    | [] -> Some j
    | k :: rest -> ( match j with `Assoc kvs -> ( match List.assoc_opt k kvs with Some v -> go v rest | None -> None) | _ -> None)
  in
  match go j path with
  | Some (`Int i) -> i
  | Some (`Float f) -> int_of_float f
  | _ -> 0

(* ---- requests ---- *)

let anthropic_blocks segs breakpoints =
  (* One text block per segment; cache_control rides the block that ends a
     breakpoint, which is how Anthropic marks a cache boundary. *)
  segs
  |> List.mapi (fun i s ->
         let base = `Assoc [ ("type", `String "text"); ("text", `String s.Segments.text) ] in
         if List.mem i breakpoints then
           match base with
           | `Assoc kvs -> `Assoc (kvs @ [ ("cache_control", `Assoc [ ("type", `String "ephemeral") ]) ])
           | other -> other
         else base)

let build_request ?(stream = false) (p : Config.provider) (plan : Cache_strategy.plan) ~model
    ~system ~messages ~max_tokens ~tools =
  let messages_json =
    List.map (fun (role, content) -> `Assoc [ ("role", `String role); ("content", `String content) ]) messages
  in
  match p.Config.wire with
  | Config.Anthropic ->
      let kvs =
        [
          ("model", `String model);
          ("max_tokens", `Int max_tokens);
          ("system", `String system);
          ("messages", `List messages_json);
        ]
        @ (if tools = [] then [] else [ ("tools", `List tools) ])
        @ if stream then [ ("stream", `Bool true) ] else []
      in
      `Assoc kvs
  | Config.Openai ->
      let sys = `Assoc [ ("role", `String "system"); ("content", `String system) ] in
      let kvs =
        [
          ("model", `String model);
          ("max_tokens", `Int max_tokens);
          ("messages", `List (sys :: messages_json));
        ]
        @ (if tools = [] then [] else [ ("tools", `List tools) ])
        (* without this the final chunk carries no usage and the row records
           zero tokens for a call that was billed *)
        @ if stream then [ ("stream", `Bool true); ("stream_options", `Assoc [ ("include_usage", `Bool true) ]) ]
          else []
      in
      ignore plan;
      `Assoc kvs

(* The segment list is flattened into the system prompt for providers with a
   single system slot.  The breakpoints are expressed by the caller through
   [anthropic_system_blocks] when it wants them honoured. *)
let anthropic_system_blocks (segs : Segments.t) (plan : Cache_strategy.plan) =
  anthropic_blocks segs plan.Cache_strategy.breakpoints

(* ---- responses ---- *)

let parse_anthropic (j : Yojson.Safe.t) =
  let content = match j with `Assoc kvs -> (match List.assoc_opt "content" kvs with Some (`List l) -> l | _ -> []) | _ -> [] in
  let text =
    content
    |> List.filter_map (function
         | `Assoc kvs -> ( match List.assoc_opt "type" kvs with Some (`String "text") -> ( match List.assoc_opt "text" kvs with Some (`String t) -> Some t | _ -> None) | _ -> None)
         | _ -> None)
    |> String.concat ""
  in
  let tool_calls =
    content
    |> List.filter_map (function
         | `Assoc kvs -> (
             match List.assoc_opt "type" kvs with
             | Some (`String "tool_use") ->
                 let name = match List.assoc_opt "name" kvs with Some (`String n) -> n | _ -> "" in
                 let args = match List.assoc_opt "input" kvs with Some a -> a | None -> `Null in
                 Some { name; args }
             | _ -> None)
         | _ -> None)
  in
  let usage =
    {
      input = opt_int j [ "usage"; "input_tokens" ];
      output = opt_int j [ "usage"; "output_tokens" ];
      cache_write = opt_int j [ "usage"; "cache_creation_input_tokens" ];
      cache_read = opt_int j [ "usage"; "cache_read_input_tokens" ];
    }
  in
  let stop = match j with `Assoc kvs -> (match List.assoc_opt "stop_reason" kvs with Some (`String s) -> Some s | _ -> None) | _ -> None in
  Ok { text = (if text = "" then None else Some text); tool_calls; usage; stop }

let parse_openai (j : Yojson.Safe.t) =
  let choice = match j with `Assoc kvs -> (match List.assoc_opt "choices" kvs with Some (`List (c :: _)) -> Some c | _ -> None) | _ -> None in
  let msg = match choice with Some c -> (match c with `Assoc kvs -> List.assoc_opt "message" kvs | _ -> None) | None -> None in
  let text =
    match msg with
    | Some (`Assoc kvs) -> ( match List.assoc_opt "content" kvs with Some (`String s) when s <> "" -> Some s | _ -> None)
    | _ -> None
  in
  let tool_calls =
    match msg with
    | Some (`Assoc kvs) -> (
        match List.assoc_opt "tool_calls" kvs with
        | Some (`List l) ->
            List.filter_map
              (fun tc ->
                match tc with
                | `Assoc tkvs -> (
                    let fn = match List.assoc_opt "function" tkvs with Some f -> f | None -> `Null in
                    match fn with
                    | `Assoc fkvs ->
                        let name = match List.assoc_opt "name" fkvs with Some (`String n) -> n | _ -> "" in
                        let args =
                          match List.assoc_opt "arguments" fkvs with
                          | Some (`String s) -> ( try Yojson.Safe.from_string s with _ -> `String s)
                          | Some other -> other
                          | None -> `Null
                        in
                        Some { name; args }
                    | _ -> None)
                | _ -> None)
              l
        | _ -> [])
    | _ -> []
  in
  let prompt = opt_int j [ "usage"; "prompt_tokens" ] in
  let cached = opt_int j [ "usage"; "prompt_tokens_details"; "cached_tokens" ] in
  let usage =
    {
      input = max 0 (prompt - cached);
      output = opt_int j [ "usage"; "completion_tokens" ];
      cache_write = 0;
      cache_read = cached;
    }
  in
  let stop = match choice with Some (`Assoc kvs) -> (match List.assoc_opt "finish_reason" kvs with Some (`String s) -> Some s | _ -> None) | _ -> None in
  Ok { text; tool_calls; usage; stop }

let parse_response (p : Config.provider) j =
  match p.Config.wire with Config.Anthropic -> parse_anthropic j | Config.Openai -> parse_openai j

(* ---- cost ---- *)

let cost_usd (p : Config.provider) (u : usage) =
  let m = 1_000_000.0 in
  let pr = p.Config.prices in
  let per tok rate = float_of_int tok *. rate /. m in
  match p.Config.wire with
  | Config.Anthropic ->
      per u.input pr.Config.input
      +. per u.output pr.Config.output
      +. per u.cache_write (pr.Config.input *. pr.Config.cache_write_mult)
      +. per u.cache_read (pr.Config.input *. pr.Config.cache_read_mult)
  | Config.Openai ->
      (* the write is not an additive fee; cached input bills at the read rate *)
      per u.input pr.Config.input
      +. per u.output pr.Config.output
      +. per u.cache_read (pr.Config.input *. pr.Config.cache_read_mult)

(* ---------- streaming ----------

   Both wires stream the same conceptual thing with different addresses:

     Anthropic  named events (message_start carries the input and cache
                counters, content_block_* carries text and tool_use, and
                message_delta carries the output count and stop reason)
     OpenAI     anonymous chunks whose delta carries content and tool_calls
                fragments, with usage only in the final chunk when
                stream_options.include_usage is set

   Tool arguments arrive as fragments of a JSON string and are only valid once
   the stream says the block is done, so the assembler concatenates and parses
   at the end: an argument blob is data, never something to evaluate early. *)

type stream = {
  buf : Buffer.t;
  tools : (int, string * Buffer.t) Hashtbl.t;
  mutable order : int list;
  mutable usage : usage;
  mutable stop : string option;
}

let new_stream () =
  {
    buf = Buffer.create 256;
    tools = Hashtbl.create 4;
    order = [];
    usage = usage_zero;
    stop = None;
  }

let tool_slot st index =
  match Hashtbl.find_opt st.tools index with
  | Some slot -> slot
  | None ->
      let slot = ("", Buffer.create 64) in
      Hashtbl.add st.tools index slot;
      st.order <- st.order @ [ index ];
      slot

let feed st (p : Config.provider) (j : Yojson.Safe.t) =
  match p.Config.wire with
  | Config.Anthropic -> (
      match opt_string_field "type" j with
      | Some "message_start" ->
          st.usage <-
            {
              input = opt_int j [ "message"; "usage"; "input_tokens" ];
              output = 0;
              cache_write = opt_int j [ "message"; "usage"; "cache_creation_input_tokens" ];
              cache_read = opt_int j [ "message"; "usage"; "cache_read_input_tokens" ];
            }
      | Some "content_block_start" -> (
          match field "content_block" j with
          | Some cb when opt_string_field "type" cb = Some "tool_use" ->
              let index = opt_int j [ "index" ] in
              let name = match opt_string_field "name" cb with Some n -> n | None -> "" in
              ignore (tool_slot st index);
              Hashtbl.replace st.tools index (name, snd (tool_slot st index))
          | _ -> ())
      | Some "content_block_delta" -> (
          let index = opt_int j [ "index" ] in
          match field "delta" j with
          | Some d -> (
              match opt_string_field "type" d with
              | Some "text_delta" ->
                  (match opt_string_field "text" d with
                  | Some t -> Buffer.add_string st.buf t
                  | None -> ())
              | Some "input_json_delta" -> (
                  match opt_string_field "partial_json" d with
                  | Some frag -> Buffer.add_string (snd (tool_slot st index)) frag
                  | None -> ())
              | _ -> ())
          | None -> ())
      | Some "message_delta" ->
          st.usage <- { st.usage with output = opt_int j [ "usage"; "output_tokens" ] };
          (match field "delta" j with
          | Some d -> ( match opt_string_field "stop_reason" d with Some s -> st.stop <- Some s | None -> ())
          | None -> ())
      | _ -> ())
  | Config.Openai -> (
      (match field "usage" j with
      | Some `Null | None -> ()
      | Some _ ->
          let prompt = opt_int j [ "usage"; "prompt_tokens" ] in
          let cached = opt_int j [ "usage"; "prompt_tokens_details"; "cached_tokens" ] in
          st.usage <-
            {
              input = max 0 (prompt - cached);
              output = opt_int j [ "usage"; "completion_tokens" ];
              cache_write = 0;
              cache_read = cached;
            });
      match field "choices" j with
      | Some (`List (choice :: _)) -> (
          (match field "delta" choice with
          | Some d -> (
              (match opt_string_field "content" d with
              | Some t -> Buffer.add_string st.buf t
              | None -> ());
              match field "tool_calls" d with
              | Some (`List tcs) ->
                  List.iter
                    (fun tc ->
                      let index = opt_int tc [ "index" ] in
                      let name, args = tool_slot st index in
                      let name =
                        match field "function" tc with
                        | Some f -> (
                            match opt_string_field "name" f with Some n -> n | None -> name)
                        | None -> name
                      in
                      (match field "function" tc with
                      | Some f -> (
                          match opt_string_field "arguments" f with
                          | Some frag -> Buffer.add_string args frag
                          | None -> ())
                      | None -> ());
                      Hashtbl.replace st.tools index (name, args))
                    tcs
              | _ -> ())
          | None -> ());
          match opt_string_field "finish_reason" choice with
          | Some s -> st.stop <- Some s
          | None -> ())
      | _ -> ())

let feed_events st p events =
  List.iter
    (fun (e : Sse.event) ->
      if e.Sse.data <> "" && e.Sse.data <> "[DONE]" then
        match Yojson.Safe.from_string e.Sse.data with
        | exception Yojson.Json_error _ -> ()
        | j -> feed st p j)
    events

let finish_stream st =
  let tool_calls =
    st.order
    |> List.filter_map (fun index ->
           match Hashtbl.find_opt st.tools index with
           | None -> None
           | Some (name, args) ->
               let json = Buffer.contents args in
               let parsed =
                 if json = "" then `Null
                 else (try Yojson.Safe.from_string json with _ -> `String json)
               in
               Some { name; args = parsed })
  in
  let text = Buffer.contents st.buf in
  { text = (if text = "" then None else Some text); tool_calls; usage = st.usage; stop = st.stop }

let assemble_stream p (body : string) =
  let st = new_stream () in
  feed_events st p (Sse.parse body);
  finish_stream st
