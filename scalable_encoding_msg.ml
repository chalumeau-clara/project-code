(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits =
   let length = String.length str -1 in
  let p a = int_of_char str.[a] in 

  let rec zero = function
    p when p <= 0 -> ""
        | p -> "0" ^ zero (p-1) in 

  let rec dectobin bits = function
  n  when n <= 0 -> zero bits
    |n -> string_of_int (n mod 2)  ^ dectobin (bits-1) (n/2)  in

  let rec concatene = function
  t when t <0  -> ""
    |t -> dectobin bits (p t) ^ concatene (t-1)

  in let rec bintodec b a = function
     s when (String.length s -1) < b -> []
    |s when (s.[b] = '1') -> Scalable.add_b ((Scalable_power.power [0;0;1] a)) ( bintodec (b+1) (Scalable.add_b a [0;1]) s)
    |s  ->  bintodec (b+1) (Scalable.add_b a [0;1]) s
     in 
     bintodec 0 [] (concatene length);;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let decode msg bits =

  let power x n =
  let d = n in 
    let rec power2 = function
       (x,n) when n < 2 -> if d = 0 then 1 else x
      |(x,n) when 2 mod n = 0 -> power2 (x*x,n/2)
      |(x,n) -> x* power2 (x*x,(n-1)/ 2)
    in power2 (x,n)
  in 
 let rec zero = function
    p when p <= 0 -> ""
        | p -> "0" ^ zero (p-1) in 

 let rec dectobin bits = function
   [] -> zero bits
    |e::l -> string_of_int e  ^ dectobin (bits-1) l  in

 let rec bintodec b = function
     s when ( String.length s -1) < b -> 0
    |s when (s.[b] = '1') -> ( power 2 b) +  bintodec (b+1) s
    |s  ->  bintodec (b+1) s in 

 let rec par7bits b n = function
     s when (String.length s -1) <=  b-> n
    |s ->  par7bits (b+7) (Char.escaped (char_of_int (bintodec 0 (String.sub s b 7))) ^ n) s 
  in par7bits 0 "" (dectobin bits msg) ;;
