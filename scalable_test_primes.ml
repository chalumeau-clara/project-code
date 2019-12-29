(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n =
  let rec prime d = 
    if Scalable.(>>) (Scalable.mult_b d d) n then true
      else
	if (Scalable.mod_b n d) = [] then false
	else prime (Scalable.add_b d [0;1])
    in n=[0;0;1] || prime [0;0;1];;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec composite i = if Scalable.(>>) (Scalable.mult_b i i) p then false else (if (Scalable.mod_b p i) = [] then true  else composite (Scalable.add_b i [0;1])) in 
    let rec pseudo = function
       [] -> true
      |e::l when Scalable_power.mod_power p (Scalable.diff_b e [0;1]) e = [0;1] && composite [0;0;1] -> false
      |e::l -> pseudo l 
    in pseudo test_seq;;
