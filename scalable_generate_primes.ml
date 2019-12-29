(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics
open Test_primes

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
*)

let init_eratosthenes n =
  let n = Scalable.from_int n in 
   let rec init a = function
    n when Scalable.(>>) a n -> []
      |n -> a :: init (Scalable.add_b a [0;0;1]) n
    in [0;0;1]::init [0;1;1] n ;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let n = Scalable.from_int n in 
   let rec enleve_multiple n = function
    [] -> []
      |e::l -> if Scalable.mod_b e n = [] then enleve_multiple n l
	else e:: enleve_multiple n l
    in let rec eras = function
    [] -> []
      |e::l when Scalable.(>>) (Scalable.mult_b e e) n -> e::l
      |e::l -> e:: eras (enleve_multiple e l)
       in eras(init_eratosthenes (Scalable.to_int n));;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime bitarrays for specific or fun
   purposes.
 *)

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let limit = Scalable.from_int limit in
   let rec double = function
        i when i = limit -> []
      | i when (Scalable_test_primes.is_prime i) && (Scalable_test_primes.is_prime (Scalable.add_b (Scalable.mult_b [0;0;1] i) [0;1])) -> (i,(Scalable.add_b (Scalable.mult_b [0;0;1] i) [0;1])) :: double (Scalable.add_b i [0;1])
      |i -> double (Scalable.add_b i [0;1])
    in double [0;0;1];;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let limit = Scalable.from_int limit in
    let rec twin = function
        i when i = limit -> []
      | i when (Scalable_test_primes.is_prime i) && (Scalable_test_primes.is_prime (Scalable.add_b [0;0;1] i)) -> (i,Scalable.add_b [0;0;1] i) :: twin (Scalable.add_b i [0;1])
      |i -> twin (Scalable.add_b i [0;1])
    in twin [0;0;1];;
