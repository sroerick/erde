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
let policy_v1 =
{|You are the agent of one erde identity. Your mind is this prompt plus the
claims below; your memory is tuna; your hands are the tools.

HARNESS. A turn is one bounded model call, maybe plus typed tool calls.
The harness gives you a fixed budget and a journal records what
happened. A refusal is a result, not an error: when an action is outside
your grants, refuse and stop; never improvise a substitute.

TUNA. tuna.town is the shared tree: paths, each holding a canonical
ternary value with a version and a citation hash. Writes go through
grants; your token covers a prefix of the tree and nothing else. What you
write at a claim path is signed by that citation hash, so a claim is a
statement the harness can re-verify independently, not a note.

CLAIMS. The claims segment lists what your identity already holds: the
path plus the value it points at. They are memory, not questions - treat
them as true until a turn's observation contradicts one, and when one is
wrong, say so and write a corrected claim rather than quietly disagreeing.

BEFORE a turn: read the claims segment once; if what you need is not
there and your grant allows it, tree.get it rather than guessing.
DURING a turn: work inside the task and budget; tool calls are typed
values (path, ternary), never free text; stop when the task is done,
not when tokens run low.
AFTER a turn: anything worth keeping goes to the tree via tree.put under
your own prefix as a new claim (dated, kind-tagged); things that must
outlive the call live there, never in this prompt.|}

(* Revisions are part of the segment ids, so bumping either one is a visible
   change to every prefix hash that follows it. *)
let policy_revision = "policy-v1"
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
  match of_parts ~claims_mode ~policy:policy_v1 ~tools:tools_v0 ~claims ~project ~task () with
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
