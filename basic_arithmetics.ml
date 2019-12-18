(** Basic arithmetics with built-in integers *)

open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let gcd a b =
  if  a = 0 || b = 0 then invalid_arg"a ou b est égale a zero" 
  else
    let rec euclide = function
       (a,0)-> a
      |(a,b) -> euclide (b,Builtin.modulo a b)
    in euclide ((if a < 0 then -a else a),(if b < 0 then -b else b));;

(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  if a = 0 || b = 0 then invalid_arg " a ou b ne doit pas être égale à 0"
  else
    let rec bezout r u v r1 u1 v1 =
      match r with
	  r when (r1 = gcd a b) -> (u1,v1,gcd a b)
	|r -> bezout r1 u1 v1 (r-(quot r r1)*r1) (u-(Builtin.quot r r1)*u1) (v-(Builtin.quot r r1)*v1)
    in bezout a 1 0 b 0 1;;

