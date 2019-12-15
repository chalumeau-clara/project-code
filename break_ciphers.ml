(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let (n,e) = key in
  let rec ess d = function
    |f when f*d = n -> (d,f)
    |f when d*d < n -> ess (d+1) f
    |f -> ess 2 (f+1) in 
 ess 2 2;;
