(* Erde.Prompt -- the prompt as a pure function of its inputs.

   The design's rule: prompt construction is a pure function of (claims
   revision, policy revision, tool schema revision, task), content-addressed.
   Then "why did this miss" is a diff and "reuse that prefix" is a hash.

   So this module is the one place a prompt is assembled, and it takes claims
   as data -- it never fetches them.  The identity's claims arrive from the
   authority, get sorted canonically, and become the claims segment; change one
   claim and the prefix hash changes, which is exactly the cache boundary. *)

let policy_v0 =
  "Erde microturn. A turn is one model call with a fixed budget; a tool call \
   is a typed value, never shell text; a refusal is recorded, not argued with. \
   State that must outlive the call belongs in a claim on tuna, not in this \
   prompt."

let tools_v0 = "tree.get, tree.list, tree.put (grant-gated; a call outside a grant is refused locally)"

(* Revisions are part of the segment ids, so bumping either one is a visible
   change to every prefix hash that follows it. *)
let policy_revision = "policy-v0"
let tools_revision = "tools-v0"

let of_parts ?(claims_mode = Segments.Claim_set.Cited) ~policy ~tools ~claims ~project ~task () =
  let claims_segment =
    if claims = [] then { Segments.kind = Segments.Claims; id = "claims-empty"; text = "" }
    else Segments.Claim_set.segment ~mode:claims_mode claims
  in
  Segments.make
    [
      { Segments.kind = Segments.Framework; id = policy_revision; text = policy };
      { Segments.kind = Segments.Tools; id = tools_revision; text = tools };
      claims_segment;
      { Segments.kind = Segments.Project; id = "project"; text = project };
      { Segments.kind = Segments.Task; id = "task"; text = task };
    ]

let default ?(project = "") ?(claims = []) ?(claims_mode = Segments.Claim_set.Cited) ~task () =
  match of_parts ~claims_mode ~policy:policy_v0 ~tools:tools_v0 ~claims ~project ~task () with
  | Ok t -> Ok t
  | Error m -> Error m

let parse_claims_mode = function
  | "full" -> Ok Segments.Claim_set.Full
  | "cited" -> Ok Segments.Claim_set.Cited
  | s when String.length s > 5 && String.sub s 0 5 = "trunc" -> (
      match int_of_string_opt (String.sub s 5 (String.length s - 5)) with
      | Some n when n > 0 -> Ok (Segments.Claim_set.Truncated n)
      | _ -> Error (Printf.sprintf "unknown claims mode %S" s))
  | s -> Error (Printf.sprintf "unknown claims mode %S (full|cited|truncN)" s)
