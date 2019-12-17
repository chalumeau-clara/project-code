(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)

let inverse_n l =
  let rec f d = function
[] -> d
    |e::l -> f (e::d) l
  in f [] l;;

let inverse_b l =
  let rec f d = function
[] -> d
    |e::l -> f (e::d) l
  in
  match l with
      [] -> []
    |e::l -> e:: (f l);;

let from_int x = if x = 0 then [] else let y = x in 
  let rec dectobin = function
  n  when n <= 0 -> []
    |n -> (n mod 2) :: dectobin (n/2)
  in if y < 0 then 1::dectobin (-x) else 0::dectobin x ;;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object. ()
 *)
let to_int bA =
  let rec bintodec b = function
     [] -> 0
    |e::l when e = 1 ->  b +  bintodec (2*b) l
    |e::l  ->  bintodec (b*2) l
  in
  match bA with
      [] -> 0
    |e::bA when e = 1 -> -1* bintodec 1 bA
    |e::bA -> 1* bintodec 1 bA;;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec print d = function
    []-> d
    |e::l -> (if e = 1 then
              print ("1"^d) l else
      print ("0"^d) l)
  in
  match bA with
      [] -> print_string  "0";
   |e::bA when  e = 1 -> print_string "1" ;
                           print_string (print "" bA)
    |e::bA ->  print_string "0" ;
      print_string (print "" bA) ;;

let rec compare_n nA nB =
  let a = inverse_n nA in 
  let b = inverse_n nB in
  let rec comp = function 
  ([],[]) -> 0
    |(_::_, []) -> 1
    |([], _::_) -> -1
    |(e::l,f::g) when e = f -> comp (l,g)
    |(e::l,f::g) when e > f -> 1
    |(e::l,f::g) when e < f -> -1
  in comp (a,b);;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)


let (>>!) nA nB =
if compare_n nA nB = 1 then true else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
 if compare_n nA nB = (-1) then true else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
 if compare_n nA nB = (-1) then false else true;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
 if compare_n nA nB = 1 then false else true;;


(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  match (bA,bB) with
      ([],[]) -> 0
    |(_::_, []) -> 1
    |([], _::_) -> -1
    |(e::l,f::g) -> if (<<!) l g  then -1 else (if (>>!) l g then 1 else 0);;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
   let a = to_int nA in
  let b = to_int nB in
  if a >  b then true else false ;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  let a = to_int nA in
  let b = to_int nB in
  if a >= b then true else false ;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  let a = to_int nA in
  let b = to_int nB in
  if a <=  b then true else false ;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
  let a = to_int nA in
  let b = to_int nB in
  if a >= b then true else false ;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
      [] -> 1
    |e::l when e = 1 -> -1
    |e::l-> 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
   match bA with
      [] -> []
    |e::l when e = 1 -> 0::l
    |e::l-> e::l;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (0, 0)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = []

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB = []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
