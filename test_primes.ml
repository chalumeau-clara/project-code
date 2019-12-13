(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power


(** Deterministic primality test *)
let is_prime n =
  if n < 2 then invalid_arg "n doit être sup à 2"
  else
    let rec prime d =
      if d*d > n then true
      else
	if modulo n d =0 then false
	else prime (d+1)
    in n=2 || prime 2;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq =
  if p < 2 then invalid_arg "p plus petit que 2"
  else
    let rec pseudo = function
    [] -> true
      |e::l when (modulo (power e p) p) = e -> pseudo l
      |e::l -> false
    in pseudo test_seq;;
