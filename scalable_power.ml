(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let rec pow x n =
   match n with
      []-> [0;1]
    |n -> Scalable.mult_b x (pow x (Scalable.diff_b n [0;1]));;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  let d = n in 
  let rec power2 = function
      (_::_, []) -> [0;1]
      |([], _)-> []
      |(x,n) when (Scalable.(<<) n [0;0;1])  -> if d = [] then [0;1] else x
      |(x,n) when ((Scalable.compare_b (Scalable.mod_b n [0;0;1]) []) = 0) -> power2 (Scalable.mult_b x x ,Scalable.quot_b n [0;0;1])
      |(x,n) -> Scalable.mult_b x (power2 (Scalable.mult_b x x ,Scalable.quot_b (Scalable.diff_b n  [0;1]) [0;0;1]))
    in power2 (x,n);;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m = 
  let d = n in let e = x in 
    let rec modpow = function
    (_::_, []) -> [0;1]
      |([], _)-> []
      | (x,n) when (Scalable.(<<) n [0;0;1]) -> if d = [] then (if e = [] then [] else [0;1]) else Scalable.mod_b x m
      |(x,n) when (Scalable.mod_b n [0;0;1]) = [] -> modpow (Scalable.mod_b (Scalable.mult_b x x) m ,Scalable.quot_b n [0;0;1])
      |(x,n) -> Scalable.mod_b (Scalable.mult_b x (modpow (Scalable.mod_b (Scalable.mult_b x x) m ,Scalable.quot_b (Scalable.diff_b n [0;1]) [0;0;1]))) m
    in modpow (x,n);;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
   mod_power x (Scalable.mod_b n (Scalable.diff_b p [0;1])) p
