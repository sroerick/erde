(* Erde.Treecalc -- tree calculus, the arithmetic under a claim value.

   Faithful port of the S1-S4 interpreter (spec ~/swarm-demo/treecalc/PLAN.md;
   PP port libs/treecalc/lib.pp, whose vectors were cross-checked against
   Johannes Bader's eager-stacks oracle). Same representation, same eager
   order:

   In the source a tree IS the list of its children, head = leftmost child:
   leaf = (), stem = (u), fork = (a b); applying e to f attaches f as the
   right-most child of e's root.  OCaml cannot spell that as an
   abbreviation, so [T] wraps that list; unwrap wherever the source says
   "children".

   Reduction rules (x y z = leftmost three children, r = the rest):
     (1)  x leaf                  -> kids y ++ r
     (2)  x stem(w)               -> kids w ++ (z :: (y z) :: r); the (y z)
         redex reduces fully before the parent re-fires
     (3a) x fork(w _), z leaf     -> kids w ++ r
     (3b) x fork(w x2), z stem(u) -> kids x2 ++ (u :: r)
     (3c) x fork, z fork(u v)     -> kids y ++ (u :: v :: r)
   Open children (>= 3) reduce on demand exactly where the pseudocode says
   "no rule fires now; z reduces via its own entry". *)

type t = T of t list  (* T xs = the tree whose children are xs; T [] = leaf *)

exception Out_of_steps

let default_limit = 4000

(* ---------- ternary: preorder arity encoding, whitespace tolerant ---------- *)

let from_ternary s =
  let n = String.length s in
  let rec go i =
    if i >= n then Error "tc: truncated ternary" else
    match s.[i] with
    | '0' -> Ok (T [], i + 1)
    | '1' -> (
        match go (i + 1) with
        | Ok (u, i2) -> Ok (T [u], i2)
        | Error e -> Error e)
    | '2' -> (
        match go (i + 1) with
        | Ok (a, i2) -> (
            match go i2 with
            | Ok (b, i3) -> Ok (T [a; b], i3)
            | Error e -> Error e)
        | Error e -> Error e)
    | ' ' | '\t' | '\n' | '\r' -> go (i + 1)
    | _ -> Error "tc: bad ternary char"
  in
  match go 0 with
  | Ok (v, i) when i = n -> Ok v
  | Ok _ -> Error "tc: trailing ternary chars"
  | Error e -> Error e

let rec to_buf buf (T xs) =
  match xs with
  | [] -> Buffer.add_char buf '0'
  | [ one ] -> Buffer.add_char buf '1'; to_buf buf one
  | [ a; b ] -> Buffer.add_char buf '2'; to_buf buf a; to_buf buf b
  | _ -> assert false

let to_ternary (T _ as v) =
  let buf = Buffer.create 64 in
  to_buf buf v;
  Buffer.contents buf

(* ---------- human-readable text

   node -> "t", then per child " " + wrap(child); a non-leaf child is
   parenthesized because a bare second token would re-associate. ---------- *)

let rec to_text v =
  if v = T [] then "t"
  else
    let kid k = if k = T [] then "t" else "(" ^ to_text k ^ ")" in
    let T xs = v in
    List.fold_left (fun acc k -> acc ^ " " ^ kid k) "t" xs

(* ---------- naturals: natural n is a chain of n stems (0 = leaf) ---------- *)

let rec nat n = if n = 0 then T [] else T [ nat (n - 1) ]

let rec as_nat (T xs) =
  match xs with
  | [] -> Some 0
  | [ u ] -> Option.map succ (as_nat u)
  | _ -> None

(* A tree that is a list of nat chains reads as char codes. *)
let as_text (v : t) =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | u :: rest -> (match as_nat u with Some n -> go (n :: acc) rest | None -> None)
  in
  match go [] (match v with T xs -> xs) with
  | None -> None
  | Some ns when List.for_all (fun n -> n < 127) ns ->
      Some (String.concat "" (List.map (fun n -> String.make 1 (Char.chr n)) ns))
  | _ -> None

(* Cons-list decode: fork(a, b) chains where a is a nat chain, b is the rest. *)
let as_cons_nats (v : t) : int list option =
  let rec go acc v =
    match v with
    | T [] -> Some (List.rev acc)
    | T [ a; b ] -> (match as_nat a with Some n -> go (n :: acc) b | None -> None)
    | _ -> None
  in go [] v

let as_cons_text (v : t) =
  match as_cons_nats v with
  | Some ns when ns <> [] && List.for_all (fun n -> n < 127 && n > 0) ns ->
      Some (String.concat "" (List.map (fun n -> String.make 1 (Char.chr n)) ns))
  | _ -> None

(* ---------- evaluation

   A spine is an application chain: a list of children whose length is the
   arity + 2. eval returns the reduced children list (a value when its
   length < 3) and the number of tree-calculus steps consumed. *)

let to_kids (T xs) = xs
let of_kids xs = T xs

let rec eval_spine (spine : t list) steps limit =
  if List.length spine < 3 then (spine, steps)
  else if steps >= limit then raise Out_of_steps
  else
    let x = List.nth spine 0 in
    let y = List.nth spine 1 in
    let z = List.nth spine 2 in
    let r = match spine with _ :: _ :: _ :: tail_rest -> tail_rest | _ -> assert false in
    let lx = to_kids x |> List.length and lz = to_kids z |> List.length in
    let ky = to_kids y in
    if lx < 3 then
      if lx = 0 then eval_spine (ky @ r) (steps + 1) limit
      else if lx = 1 then (
        match x with
        | T [ w ] ->
            (* rule 2: the (y z) redex reduces fully first *)
            let rz, rs = eval_spine (ky @ [z]) (steps + 1) limit in
            eval_spine (to_kids w @ (z :: (of_kids rz :: r))) rs limit
        | _ -> assert false)
      else
        match x with
        | T [ w; x2 ] -> (
            (* rule 3: x is a fork; triage on z *)
            let kw = to_kids w and kx2 = to_kids x2 in
            if lz = 0 then eval_spine (kw @ r) (steps + 1) limit
            else if lz = 1 then
                (match z with
                | T [ u ] -> eval_spine (kx2 @ (u :: r)) (steps + 1) limit
                | _ -> assert false)
            else
              match z with
              | T [ u; v ] -> eval_spine (ky @ (u :: v :: r)) (steps + 1) limit
              | _ ->
                  (* z is open: reduce it on its own entry, then re-dispatch *)
                  let rz, rs = eval_spine (to_kids z) steps limit in
                  eval_spine (x :: y :: (of_kids rz :: r)) rs limit)
        | _ -> assert false
    else
      (* x is open: reduce it first, then re-dispatch *)
      let rx, rs = eval_spine (to_kids x) steps limit in
      eval_spine (of_kids rx :: (y :: z :: r)) rs limit

let eval ?(limit = default_limit) (program : t list) :
    ((t list * int), string) result =
  try
    let v, steps = eval_spine program 0 limit in
    Ok (v, steps)
  with Out_of_steps -> Error "tc: out-of-steps"

(* program applied to argument: argument as the right-most child *)
let apply program arg = eval (program @ [ arg ])
