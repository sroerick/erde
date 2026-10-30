(* treecalc vectors -- ported from ~/swarm-demo/treecalc (oracle: Johannes
   Bader's eager-stacks reference evaluator).

   Representation: a tree is the list of its children, wrapped as [T]; a
   spine element is a [t].  A program is an application chain; a value is a
   spine of length < 3. *)

open Erde

let tern s = match Treecalc.from_ternary s with Ok v -> v | Error e -> failwith ("from_ternary " ^ s ^ ": " ^ e)

let kids s = match tern s with Treecalc.T ks -> ks

let ev program =
  match Treecalc.eval program with
  | Ok (v, steps) -> (Treecalc.to_ternary (Treecalc.of_kids v), steps)
  | Error e -> failwith ("eval: " ^ e)

let test_roundtrips () =
  Alcotest.(check string) "stem" "10" (Treecalc.to_ternary (tern "10"));
  Alcotest.(check string) "fork" "2100" (Treecalc.to_ternary (tern "2100"));
  Alcotest.(check string) "identity" "21100" (Treecalc.to_ternary (tern "21100"))

(* from-text "t t t t" and "t t t t t": rule 1 eats the chain. *)
let test_tri_chains () =
  let tri = tern "0" in
  Alcotest.(check string) "four" "0" (fst (ev [ tri; tri; tri ]));
  Alcotest.(check string) "five" "10" (fst (ev [ tri; tri; tri; tri ]))

(* from-ternary "21100" (identity) evaluated alone: already a value. *)
let test_identity_zero_steps () =
  match Treecalc.eval (kids "21100") with
  | Ok (_, steps) -> Alcotest.(check int) "zero steps" 0 steps
  | Error e -> failwith e

(* S K K applied to natural 3 -> natural 3. *)
let test_skk () =
  let k = tern "10" and n3 = Treecalc.nat 3 in
  Alcotest.(check string) "skk 3 = 3" "1110" (fst (ev (kids "212000" @ [ k; k; n3 ])))

(* K applied to one argument: K a spine = [t; a] -- still a 2-wide value. *)
let test_k_one () =
  let k = tern "10" in
  (* a 2-wide application spine is already a value: no rule fires. *)
  Alcotest.(check string) "k a" "2102100" (fst (ev [ k; tern "2100" ]))

let test_omega_budget () =
  let i = tern "21100" in
  let m = kids "212000" @ [ i; i ] in
  match Treecalc.eval ~limit:300 (m @ [ Treecalc.T m ]) with
  | Ok _ -> failwith "omega: expected out-of-steps, produced a value"
  | Error e -> Alcotest.(check string) "out-of-steps" "tc: out-of-steps" e

let test_to_text () =
  (* identity renders per the text grammar: root t, stem child, leaf child *)
  Alcotest.(check string) "fork text" "t (t t) t" (Treecalc.to_text (tern "2100"))

let test_chunked_roundtrip () =
  let s = "chunked-claim format v1: slice text, 600 chars max, count on the base path." in
  let tern = Treecalc.to_ternary (Treecalc.chunked_text s) in
  match Treecalc.from_ternary tern with
  | Ok v -> Alcotest.(check (option string)) "roundtrip" (Some s) (Treecalc.as_chunked_text v)
  | Error e -> failwith e

let test_as_nat () =
  Alcotest.(check (option int)) "nat 5" (Some 5) (Treecalc.as_nat (Treecalc.nat 5))

let test_as_text () =
  Alcotest.(check (option string)) "AB" (Some "AB") (Treecalc.as_text (Treecalc.T [ Treecalc.nat 65; Treecalc.nat 66 ]))

let () =
  Alcotest.run "erde.treecalc"
    [
      ( "treecalc",
        [
          Alcotest.test_case "ternary roundtrips" `Quick test_roundtrips;
          Alcotest.test_case "tri chains" `Quick test_tri_chains;
          Alcotest.test_case "identity zero steps" `Quick test_identity_zero_steps;
          Alcotest.test_case "skk is identity" `Quick test_skk;
          Alcotest.test_case "k applied once" `Quick test_k_one;
          Alcotest.test_case "omega trips the budget" `Quick test_omega_budget;
          Alcotest.test_case "to_text" `Quick test_to_text;
          Alcotest.test_case "as_nat" `Quick test_as_nat;
          Alcotest.test_case "chunked roundtrip" `Quick test_chunked_roundtrip;
          Alcotest.test_case "as_text" `Quick test_as_text;
        ] );
    ]
