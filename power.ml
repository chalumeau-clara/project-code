(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics



(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let rec pow x n =
  match n with
      0-> 1
    |n -> x * pow x (n-1);;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n < 0 then failwith "n plus petit que 0"
  else let d = n in 
    let rec power2 = function
       (x,n) when n < 2 -> if d = 0 then 1 else x
      |(x,n) when (modulo n 2) = 0 -> power2 (x*x,quot n 2)
      |(x,n) -> x* power2 (x*x,quot (n-1) 2)
    in power2 (x,n);;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = 
  let d = n in let e = x in 
    let rec modpow = function
       (x,n) when n < 2 -> if d = 0 then (if e = 0 then 0 else 1) else modulo x m
      |(x,n) when (modulo n 2) = 0 -> modpow (modulo (x*x) m ,quot n 2)
      |(x,n) -> modulo (x* (modpow (modulo (x*x) m ,quot (n-1) 2))) m
    in modpow (x,n);;
(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)

let prime_mod_power x n p =
  if n < 0 then failwith "n plus petit que 0"
   else  mod_power x (modulo n (p-1)) p ;;
