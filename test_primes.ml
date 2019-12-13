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
 let rec composite i = if i*i>p then false else if (modulo p i = 0) then true  else composite (i+1) in 
    let rec pseudo = function
       [] -> true
      |e::l when mod_power p (e-1) e = 1 && composite 2 -> false
      |e::l -> pseudo l 
    in pseudo test_seq;;

