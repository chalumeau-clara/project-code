(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  let rec euclide = function
  ([],[]) -> []
    |((e::l, [])|([], e::l))-> e::l
    |(e::l,f::g) -> euclide (f::g, Scalable.mod_b (e::l) (f::g))
  in euclide (Scalable.abs_b bA,Scalable.abs_b bB);;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
    let rec bezout r u v r1 u1 v1 =
      match r with
          []->([],[],[])
	|e::l  when (Scalable.compare_b r1 (gcd_b bA bB) = 0) -> (u1,v1,gcd_b bA bB)
	|e::l -> bezout r1 u1 v1 (Scalable.diff_b (e::l) (Scalable.mult_b (Scalable.quot_b (e::l) r1) r1)) (Scalable.diff_b u (Scalable.mult_b (Scalable.quot_b (e::l) r1) u1)) (Scalable.diff_b v (Scalable.mult_b (Scalable.quot_b (e::l) r1) v1))
    in bezout bA [0;1] [] bB [] [0;1];;
