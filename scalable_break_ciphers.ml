(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,e) = key in
  let rec ess d = function
    |f when Scalable.compare_b (Scalable.mult_b f d) n = 0  -> (d,f)
    |f when Scalable.(<<) (Scalable.mult_b d d) n -> ess (Scalable.add_b d [0;1] ) f
    |f -> ess [0;0;1] (Scalable.add_b f [0;1]) in 
 ess [0;0;1] [0;0;1];;
