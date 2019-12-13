(** Tweaking OCaml built-in euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x =
    if x < 0 then -1 else 1;;


(* Integer quotient implementation ; main use is in case of quotient
   of an integer by a natural number.
 *)

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
*)

let  modulo a b =
  if b = 0 then invalid_arg  "b positif"
  else let c = a in 
    let rec modu = function
      a when (a<b) -> if a <> 0 then (if c < 0 then b-a else a) else a
    |a -> modu (a-b)
       in modu (if a < 0 then -a else a) ;; 

let quot a b =
  if b = 0 then invalid_arg " Division par 0"
  else
    let rec quo = function 
      | a when (a < b) -> 0
      | a -> 1 + quo (a-b)
    in if a < 0 then (if (modulo a b) = 0 then 0 else -1) -(quo (if a < 0 then -a else a)) else quo (if a < 0 then -a else a) ;;

(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Modulo of two integers.
    Following Euclidean division. NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    @param a input integer
    @param b moduli a natural number.
 *)

(* Integer modulo implementations. Negative case need be taken into
   account ; representant is expected non-negative. This is not OCAML
   default.
 *)

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b =
  (quot a b, modulo a b);;
