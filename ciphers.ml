(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power
open Generate_primes
(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let rec encrypt_cesar k m b =
  if m = [] then invalid_arg " la liste ne doit pas être vide"
  else let rec encesar = function
     [] -> []
    |e::l-> modulo (k+e) b :: encesar l
       in encesar m;;



(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b =
    if m = [] then invalid_arg " la liste ne doit pas être vide"
  else let rec encesar = function
     [] -> []
    |e::l-> modulo (e-k) b :: encesar l
       in encesar m;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
 let n = p*q in
 let phin = (p-1)*(q-1)
   in let e = 65537
  in let (d,v1,gcd) = bezout e phin
     in (n,e),(n, d);;

 (*let e = let rec fonction = function
    |d when gcd phi d = 1-> d
    |d -> fonction (d+1)
  in fonction (2)*)

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = 
mod_power m e n;;


(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;
 

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
*)

let rec public_data_g p =
   let g = Random.int p in 
  match p with
    |p when modulo (g*g) p <> 1 -> (g,p)
    |p -> public_data_g p ;;
(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
*)

let generate_keys_g (g, p) =
  let a = Random.int (p-1) +1 in
  let publique = mod_power g a p in (publique,a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = Random.int (p-1) +1  in
  let c1 = mod_power g k p in
  let c2 = msg * (mod_power kA k p) in (c1,c2);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let x = mod_power msgA a p in
  let (u1,v1,gcd) = bezout x p in
  let u2 = modulo u1 p in
  let msg = modulo (u2 * msgB) p in msg;;
