(** Generating primes *)

open Builtin
open Basic_arithmetics
open Test_primes

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  if n < 2 then invalid_arg " n doit être sup à 2"
  else
    let rec init a = function
    n when a > n -> []
      |n -> a :: init (a+2) n
    in 2::init 3 n ;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  if n<2 then invalid_arg"n doit être sup à 2"
  else
    let rec enleve_multiple n = function
    [] -> []
      |e::l -> if modulo e n = 0 then enleve_multiple n l
	else e:: enleve_multiple n l
    in let rec eras = function
    [] -> []
      |e::l when e*e>n -> e::l
      |e::l -> e::eras (enleve_multiple e l)
       in eras(init_eratosthenes n);;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = 
  let oc = open_out file in
  let rec aux = function
  [] -> close_out oc
    |e::l -> Printf.fprintf oc "%s" e; aux l
  in aux li;;
(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None
(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = ()

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  if limit < 2 then invalid_arg "limit plus grand que 2"
  else
    let rec double = function
        i when i = limit -> []
      | i when (is_prime i) && (is_prime (2*i+1)) -> (i,2*i+1) :: double (i+1)
      |i -> double (i+1)
    in double 2;;
(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
   if limit < 2 then invalid_arg "limit plus grand que 2"
   else
    let rec twin = function
        i when i = limit -> []
      | i when (is_prime i) && (is_prime (2+i)) -> (i,2+i) :: twin (i+1)
      |i -> twin (i+1)
    in (2,3) :: twin 2;;
