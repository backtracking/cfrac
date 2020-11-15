(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Seq

type t = Z.t Seq.t

let terms cf = cf

let first cf = match cf () with Nil -> assert false | Cons (a0, a) -> a0, a
let int_part cf = fst (first cf)

let convergents cf =
  let rec seq hn_2 hn_1 kn_2 kn_1 cf () = match cf () with
    | Nil -> Nil
    | Cons (an, cf) ->
        let hn = Z.(an * hn_1 + hn_2) in
        let kn = Z.(an * kn_1 + kn_2) in
        Cons (Q.{ num = hn; den = kn }, seq hn_1 hn kn_1 kn cf) in
  seq Z.zero Z.one Z.one Z.zero cf

let nth_convergent n cf =
  let rec nth n cv = match cv () with
    | Nil -> invalid_arg "nth_convergent"
    | Cons (q, _) when n = 0 -> q
    | Cons (_, cv) -> nth (n - 1) cv in
  nth n (convergents cf)

let print_precision = ref 5
let set_print_precision = (:=) print_precision

let print fmt cf =
  let rec print n fmt a = match a () with
    | Nil -> ()
    | Cons _ when n = 0 -> Format.fprintf fmt "..."
    | Cons (an, a) ->
        Format.fprintf fmt "%a,@ %a" Z.pp_print an (print (n-1)) a in
  let a0, a = first cf in
  Format.fprintf fmt "[@[<hov 2> %a;@ %a@]]"
    Z.pp_print a0 (print !print_precision) a

(** {2 constructors} *)

let of_int n =
  if n < 0 then invalid_arg "of_int";
  return (Z.of_int n)

let zero = of_int 0
let one  = of_int 1

let of_z z =
  if Z.sign z < 0 then invalid_arg "of_z";
  return z

let of_q Q.{ num; den } =
  assert (Z.sign den > 0);
  let rec euclid p q =
    if q = Z.zero then empty
    else let a, r = Z.div_rem p q in
         fun () -> Cons (a, euclid q r) in
  let q, r = Z.div_rem num den in
  fun () -> Cons (q, euclid den r)

let of_float x =
  if x < 0. then invalid_arg "of_float";
  of_q (Q.of_float x)

let of_seq s =
  let check z = if Z.sign z <= 0 then invalid_arg "of_seq"; z in
  match s () with
  | Nil -> invalid_arg "of_seq"
  | Cons (z, s) -> if Z.sign z < 0 then invalid_arg "of_seq";
                   fun () -> Cons (z, Seq.map check s)

let of_fun f =
  let rec loop i () =
    let z = f i in
    if Z.sign z < 0 then invalid_arg "of_fun";
    if Z.sign z = 0 then Nil else Cons (z, loop (i + 1)) in
  fun () ->
    let z = f 0 in
    if Z.sign z < 0 then invalid_arg "of_fun";
    Cons (z, loop 1)

let rec seq_of_list l () = match l with
  | [] -> Nil
  | z :: _ when Z.sign z <= 0 -> invalid_arg "of_list"
  | z :: l -> Cons (z, seq_of_list l)

let of_list = function
  | [] -> invalid_arg "of_list"
  | z :: _ when Z.sign z < 0 -> invalid_arg "of_list"
  | z :: l -> fun () -> Cons (z, seq_of_list l)

let rec list_concat l1 s2 () = match l1 with
  | [] -> s2 ()
  | z ::  _ when Z.sign z <= 0 -> invalid_arg "periodic"
  | z :: l -> Cons (z, list_concat l s2)

let rec list_flatten f i () = match f i with
  | [] -> invalid_arg "periodic"
  | li -> list_concat li (list_flatten f (i+1)) ()

let periodic prefix f = match prefix with
  | [] -> invalid_arg "periodic"
  | z :: _ when Z.sign z < 0 -> invalid_arg "periodic"
  | z :: l -> fun () -> Cons (z, list_concat l (list_flatten f 0))

(** {2 Homographic functions (Bill Gosper, 1972)}

  Resources:
  - https://perl.plover.com/classes/cftalk/INFO/gosper.txt
  - https://github.com/mjdominus/cf/
*)

(* (a+bx)/(c+dx) *)
let homography ~a ~b ~c ~d x =
  let debug = false in
  let rec next a b c d x () =
    if debug then
      Format.eprintf "state is %a %a / %a %a@."
        Z.pp_print a Z.pp_print b Z.pp_print c Z.pp_print d;
    if c = Z.zero && d = Z.zero then
      Nil
    else
      (* use float to handle infinity *)
      let b1 = floor (Z.to_float a /. Z.to_float c) in
      let b2 = floor (Z.to_float b /. Z.to_float d) in
      if b1 = b2 then begin (* egest *)
        let q = Z.fdiv a c in
        if debug then Format.eprintf "egest %a@." Z.pp_print q;
        Cons (q, next c d Z.(a - q * c) Z.(b - q * d) x)
      end else (* ingest *)
        match x () with
        | Nil ->
            next b b d d x ()
        | Cons (p, x) ->
            if debug then Format.eprintf "ingest %a@." Z.pp_print p;
            next b Z.(a + p * b) d Z.(c + p * d) x ()
      in
  next a b c d x

let ihomography ~a ~b ~c ~d =
  homography ~a:(Z.of_int a) ~b:(Z.of_int b) ~c:(Z.of_int c) ~d:(Z.of_int d)

let inv = ihomography ~a:1 ~b:0 ~c:0 ~d:1
let zmul b = homography ~a:Z.zero ~b ~c:Z.one ~d:Z.zero
let imul b = ihomography ~a:0 ~b ~c:1 ~d:0
let zdiv x c = homography ~a:Z.zero ~b:Z.one ~c ~d:Z.zero x
let idiv x c = ihomography ~a:0 ~b:1 ~c ~d:0 x

(** {2 Some continued fractions} *)

(* phi = [1; (1)] *)
let rec phi () = Cons (Z.one, phi)

(* sqrt(2) = [1; (2)] *)
let rec constant z () = Cons (z, constant z)
let sqrt2 () = Cons (Z.one, constant (Z.of_int 2))

let pi =
  let rec cf = function
    | [] -> failwith "precision of pi exceeded"
    | ai :: a -> fun () -> Cons (Z.of_int ai, cf a) in
  cf
    (* https://oeis.org/A001203 *)
    [3; 7; 15; 1; 292; 1; 1; 1; 2; 1; 3; 1; 14; 2; 1; 1; 2; 2; 2; 2; 1; 84;
        2; 1; 1; 15; 3; 13; 1; 4; 2; 6; 6; 99; 1; 2; 2; 6; 3; 5; 1; 1; 6; 8;
        1; 7; 1; 2; 3; 7; 1; 2; 1; 1; 12; 1; 1; 1; 3; 1; 1; 8; 1; 1; 2; 1; 6;
        1; 1; 5; 2; 2; 3; 1; 2; 4; 4; 16; 1; 161; 45; 1; 22; 1; 2; 2; 1; 4;
        1; 2; 24; 1; 2; 1; 3; 1; 2; 1]

(* e = [2; 1,2,1, 1,4,1, 1,6,1, 1,8,1, ...] = [2; (1, 2n+2, 1)] *)
let e = periodic [Z.of_int 2] (fun n -> [Z.one; Z.of_int (2*n+2); Z.one])
