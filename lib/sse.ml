(* Erde.Sse -- server-sent events, as providers frame token streams.

   `data:` lines accumulate into one event until a blank line; `event:` sets
   the name; `:` starts a comment (some providers send keep-alives). *)

type event = { name : string option; data : string }

let parse (text : string) : event list =
  let events = ref [] in
  let name = ref None in
  let data = Buffer.create 256 in
  let flush () =
    if Buffer.length data > 0 then begin
      events := { name = !name; data = Buffer.contents data } :: !events;
      Buffer.clear data;
      name := None
    end
    else name := None
  in
  String.split_on_char '\n' text
  |> List.iter (fun raw ->
         let line = if String.length raw > 0 && raw.[String.length raw - 1] = '\r' then String.sub raw 0 (String.length raw - 1) else raw in
         if line = "" then flush ()
         else if line.[0] = ':' then ()
         else
           match String.index_opt line ':' with
           | Some i ->
               let field = String.sub line 0 i in
               let value =
                 let v = String.sub line (i + 1) (String.length line - i - 1) in
                 if String.length v > 0 && v.[0] = ' ' then String.sub v 1 (String.length v - 1) else v
               in
               (match field with
               | "event" -> name := Some value
               | "data" ->
                   if Buffer.length data > 0 then Buffer.add_char data '\n';
                   Buffer.add_string data value
               | _ -> ())
           | None -> ());
  flush ();
  List.rev !events
