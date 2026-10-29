(* Erde.Segments -- the prompt as an append-only list ordered by volatility.

   Cache reads are prefix reads.  Every provider keys its cache on the exact
   token prefix, so the only thing that buys a hit is: same bytes, same
   order, from the front.  Hence the shape here.

   Order is enforced, not conventional: framework, tools, claims, project,
   artifacts, task, scratch.  Editing an early segment is what destroys
   every warm entry, so the order is a checked property and the canonical
   rendering is length-prefixed, so no two different segment lists render
   to the same bytes. *)

type kind = Framework | Tools | Claims | Project | Artifacts | Task | Scratch

let kind_name = function
  | Framework -> "framework"
  | Tools -> "tools"
  | Claims -> "claims"
  | Project -> "project"
  | Artifacts -> "artifacts"
  | Task -> "task"
  | Scratch -> "scratch"

let kind_rank = function
  | Framework -> 0
  | Tools -> 1
  | Claims -> 2
  | Project -> 3
  | Artifacts -> 4
  | Task -> 5
  | Scratch -> 6

let volatility_order = "framework, tools, claims, project, artifacts, task, scratch"

type segment = { kind : kind; id : string; text : string }
type t = segment list

let make segs =
  let rec go prev acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest -> (
        match prev with
        | Some prev_kind when kind_rank s.kind < kind_rank prev_kind ->
            Error
              (Printf.sprintf "segment order: %s came after %s (order is %s)" (kind_name s.kind)
                 (kind_name prev_kind) volatility_order)
        | _ -> go (Some s.kind) (s :: acc) rest)
  in
  go None [] segs

let append t s = make (t @ [ s ])

(* Length-prefixed framing: two different segment lists never render alike. *)
let framed s = Printf.sprintf "%d:%s" (String.length s) s

let canonical t =
  t
  |> List.map (fun s -> Printf.sprintf "%s/%s/%s\n" (kind_name s.kind) s.id (framed s.text))
  |> String.concat ""

let hash_of s = Digestif.SHA256.(to_hex (digest_string s))
let prefix_hash t = hash_of (canonical t)

(* Planning device only; the journal carries the provider's real count. *)
let tokens_of_text text = (String.length text + 3) / 4
let tokens t = List.fold_left (fun acc s -> acc + tokens_of_text s.text) 0 t

(* The hash at each boundary, oldest first: breakpoint candidates, and the
   thing to diff when a cache miss needs explaining. *)
let boundaries t =
  let rec go acc cur = function
    | [] -> List.rev acc
    | s :: rest ->
        let cur = cur @ [ s ] in
        let i = List.length cur - 1 in
        go ((i, kind_name s.kind, prefix_hash cur, tokens cur) :: acc) cur rest
  in
  go [] [] t

(* claims(identity) -> segments.

   The order is canonical by construction: sorted by (path, value, source).
   Two fetches returning the same set in a different order must produce the
   same prefix hash, or every run pays full price for nothing.

   The rendering matters as much as the order.  A claim's value on tuna is a
   citation: the canonical ternary of the recorded computation, and those are
   large -- eight claims of a real garden measured 17,668 tokens, which is a
   prefix nobody wants to write on every reseal.  So a claim enters the prompt
   at one of three densities:

     Full      path = value [citation]      everything, for a reseal run
     Cited     path [citation]              the index; the value is a
                                            grant-gated tree.get away
     Truncated path = value... [citation]   a bounded taste

   Cited is the default for a turn, and it is the density that makes the tool
   host load-bearing: the prompt says what exists and where it is proven, and
   the model spends a tool call when it needs the payload. *)
module Claim_set = struct
  type claim = { path : string; value : string; source : string }
  type render_mode = Full | Cited | Truncated of int

  let compare a b = compare (a.path, a.value, a.source) (b.path, b.value, b.source)
  let canonicalize claims = List.sort compare claims

  let mode_name = function Full -> "full" | Cited -> "cited" | Truncated n -> Printf.sprintf "trunc%d" n

  let render ?(mode = Cited) claims =
    canonicalize claims
    |> List.map (fun c ->
           match mode with
           | Full -> Printf.sprintf "- %s = %s [%s]" c.path c.value c.source
           | Cited -> Printf.sprintf "- %s [%s]" c.path c.source
           | Truncated n ->
               let v = if String.length c.value <= n then c.value else String.sub c.value 0 n ^ "..." in
               Printf.sprintf "- %s = %s [%s]" c.path v c.source)
    |> String.concat "\n"

  let hash ?(mode = Cited) claims = hash_of (mode_name mode ^ "\n" ^ render ~mode claims)

  let segment ?(mode = Cited) claims =
    { kind = Claims; id = hash ~mode claims; text = render ~mode claims }
end
