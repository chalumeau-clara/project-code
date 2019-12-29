(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
   let n = Scalable.mult_b p q in
 let phin = Scalable.mult_b (Scalable.diff_b p [0;1]) (Scalable.diff_b q [0;1])
   in let e = [0;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1]
  in let (d,v1,gcd) = Scalable_basic_arithmetics.bezout_b e phin
     in (n,e),(n, d);;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
*)
let encrypt_rsa m (n, e) =
  Scalable_power.mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  Scalable_power.mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p =
  let g =
    let rec sous = function
    [] -> []
      |e::[]-> []
      |e::l -> Random.int 2 :: sous l in  sous p in (g,p);;
 (* match p with
    |p when Scalable.mod_b (Scalable.mult_b g g) p <> [0;1] -> (g,p)
    |p -> public_data_g p ;;*)

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a =
     let rec sous = function
     [] -> []
       |e::[] -> []
      |e::l -> Random.int 2 :: sous l in Scalable.add_b [0;1] (sous p) 
  in
  let publique = Scalable_power.mod_power g a p in (publique,a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k =
   let rec sous = function
    [] -> []
      |e::[]-> []
      |e::l -> Random.int 2 :: sous l in  sous p in
  let c1 = Scalable_power.mod_power g k p in
  let c2 = Scalable.mult_b msg (Scalable_power.mod_power kA k p) in (c1,c2);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
   let x = Scalable_power.mod_power msgA a p in
  let (u1,v1,gcd) = Scalable_basic_arithmetics.bezout_b x p in
  let u2 = Scalable.mod_b u1 p in
  let msg = Scalable.mod_b (Scalable.mult_b u2  msgB) p in msg;;
