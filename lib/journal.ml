(* Erde.Journal -- rows are facts, verdicts are corrections.

   Append-only, one JSON object per line.  A later row may mark an earlier
   one stale, refuted, or superseded ([Verdict], with [ref] naming the row
   it corrects) without touching it: "keep many attempts in flight and trust
   none of them" only works if the trust decision is itself recorded and
   revisable.

   Every call row carries the serving provider and the cache counters,
   because a hit rate that is not on screen is a hit rate nobody fixes. *)

type kind = Call | Turn | Verdict

let kind_name = function Call -> "call" | Turn -> "turn" | Verdict -> "verdict"

let kind_of_string = function
  | "call" -> Call
  | "turn" -> Turn
  | "verdict" -> Verdict
  | _ -> Call

type row = {
  seq : int;
  ts : float;
  kind : kind;
  identity : string;
  task : string;
  model : string;
  provider : string;
  prefix_hash : string;
  prefix_tokens : int;
  usage : Wire.usage;
  cost_usd : float;
  latency_ms : int;
  route : string;
  outcome : string;
  ref_ : string;  (* for a verdict: the seq (as string) it corrects *)
}

let to_json r =
  let u = r.usage in
  `Assoc
    [
      ("seq", `Int r.seq);
      ("ts", `Float r.ts);
      ("kind", `String (kind_name r.kind));
      ("identity", `String r.identity);
      ("task", `String r.task);
      ("model", `String r.model);
      ("provider", `String r.provider);
      ("prefix_hash", `String r.prefix_hash);
      ("prefix_tokens", `Int r.prefix_tokens);
      ("input_tokens", `Int u.Wire.input);
      ("output_tokens", `Int u.Wire.output);
      ("cache_write_tokens", `Int u.Wire.cache_write);
      ("cache_read_tokens", `Int u.Wire.cache_read);
      ("cost_usd", `Float r.cost_usd);
      ("latency_ms", `Int r.latency_ms);
      ("route", `String r.route);
      ("outcome", `String r.outcome);
      ("ref", `String r.ref_);
    ]

let str_field name j = match j with `Assoc kvs -> (match List.assoc_opt name kvs with Some (`String s) -> s | _ -> "") | _ -> ""
let int_field name j = match j with `Assoc kvs -> (match List.assoc_opt name kvs with Some (`Int i) -> i | Some (`Float f) -> int_of_float f | _ -> 0) | _ -> 0
let float_field name j = match j with `Assoc kvs -> (match List.assoc_opt name kvs with Some (`Float f) -> f | Some (`Int i) -> float_of_int i | _ -> 0.0) | _ -> 0.0

let of_json j =
  Ok
    {
      seq = int_field "seq" j;
      ts = float_field "ts" j;
      kind = kind_of_string (str_field "kind" j);
      identity = str_field "identity" j;
      task = str_field "task" j;
      model = str_field "model" j;
      provider = str_field "provider" j;
      prefix_hash = str_field "prefix_hash" j;
      prefix_tokens = int_field "prefix_tokens" j;
      usage =
        {
          Wire.input = int_field "input_tokens" j;
          output = int_field "output_tokens" j;
          cache_write = int_field "cache_write_tokens" j;
          cache_read = int_field "cache_read_tokens" j;
        };
      cost_usd = float_field "cost_usd" j;
      latency_ms = int_field "latency_ms" j;
      route = str_field "route" j;
      outcome = str_field "outcome" j;
      ref_ = str_field "ref" j;
    }

let read path =
  if not (Sys.file_exists path) then Ok []
  else
    match Config.read_file path with
    | Error m -> Error m
    | Ok text ->
        let rows =
          String.split_on_char '\n' text
          |> List.filter (fun l -> String.trim l <> "")
          |> List.filter_map (fun l ->
                 try Some (Yojson.Safe.from_string l) with _ -> None)
          |> List.filter_map (fun j -> match of_json j with Ok r -> Some r | Error _ -> None)
        in
        Ok rows

let next_seq path = match read path with Ok rows -> List.length rows | Error _ -> 0

let append ~path r =
  (* 0o600: the journal is per-identity accounting, not public. *)
  try
    let oc = open_out_gen [ Open_append; Open_creat; Open_wronly ] 0o600 path in
    output_string oc (Yojson.Safe.to_string (to_json r));
    output_char oc '\n';
    flush oc;
    close_out oc;
    Ok ()
  with Sys_error m -> Error m

(* The lever from the design: you cannot optimize a hit rate that is not on
   screen.  Per provider: calls, cache reads, cache writes, non-cached
   input, spend, and the hit share of all input tokens. *)
let hit_rates rows =
  let calls = List.filter (fun r -> r.kind = Call) rows in
  let providers = List.sort_uniq compare (List.map (fun r -> r.provider) calls) in
  List.map
    (fun p ->
      let mine = List.filter (fun r -> r.provider = p) calls in
      let sum f = List.fold_left (fun a r -> a + f r) 0 mine in
      let input = sum (fun r -> r.usage.Wire.input) in
      let read = sum (fun r -> r.usage.Wire.cache_read) in
      let write = sum (fun r -> r.usage.Wire.cache_write) in
      let out = sum (fun r -> r.usage.Wire.output) in
      let cost = List.fold_left (fun a r -> a +. r.cost_usd) 0.0 mine in
      let total_in = input + read + write in
      let share = if total_in = 0 then 0.0 else float_of_int read /. float_of_int total_in in
      Printf.sprintf
        "%-12s calls=%d in=%d cached_in=%d cache_write=%d out=%d hit=%.0f%% cost=$%.4f" p
        (List.length mine) input read write out (share *. 100.0) cost)
    providers
