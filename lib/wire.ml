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

let build_request (p : Config.provider) (plan : Cache_strategy.plan) ~model ~system ~messages
    ~max_tokens ~tools =
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
