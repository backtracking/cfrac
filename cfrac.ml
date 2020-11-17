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
(* this is nonempty sequence *)

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

let best_approx d x =
  if Z.sign d <= 0 then invalid_arg "best_approx";
  let rec lookup q cv = match cv () with
    | Nil -> q
    | Cons _ when q.Q.den >= d -> q
    | Cons (q, cv) -> lookup q cv in
  match convergents x () with
  | Nil -> assert false
  | Cons (q, cv) -> lookup q cv

(* Conversion to a float. (Algorithm by Guillaume Melquiond)
   1. find a lower bound M of x
   2. compute a convergent q of x with a denominator at least ceil(2^60/M)
      round this to the nearest float f (using Q.to_float)
   3. if the convergent q is of odd order and q >= f or
      if the convergent q is of even order and q <= f, then return f
   4. otherwise, repeat with the new convergent *)
let to_float x =
  match x () with
  | Nil -> assert false
  | Cons (a0, cf) ->
      let lookup d =
        let rec conv odd hn_2 hn_1 kn_2 kn_1 cf = match cf () with
          | Nil ->
              (* we have an exact rational value *)
              Q.(to_float { num = hn_1; den = kn_1 })
          | Cons (an, cf) ->
              (* otherwise, compute the next convergent (which is odd iff odd) *)
              let hn = Z.(an * hn_1 + hn_2) in
              let kn = Z.(an * kn_1 + kn_2) in
              if kn >= d then
                let q = Q.{ num = hn; den = kn } in
                let f = Q.to_float q in
                if if odd then Q.(q >= of_float f) else Q.(q <= of_float f) then
                  f
                else
                  conv (not odd) hn_1 hn kn_1 kn cf
              else
                conv (not odd) hn_1 hn kn_1 kn cf in
        conv true Z.one a0 Z.zero Z.one cf in
      let t60 = Z.(pow (of_int 2) 60) in
      if a0 = Z.zero then
        match cf () with
        | Nil -> 0.
        | Cons (a1, _) -> assert (Z.sign a1 > 0);
                          lookup (Z.mul t60 a1)
      else
        lookup (Z.cdiv t60 a0)

(* Another solution: convert convergents to floats, until we get twice the
   same floating point number. It happens to be slower on some cases
   (e.g. phi, sqrt2) but faster on others (e.g. pi). *)
let _to_float x =
  let rec lookup last cv = match cv () with
    | Nil -> last
    | Cons (q, cv) ->
        let f = Q.to_float q in
        if f = last then f else lookup f cv in
  match convergents x () with
  | Nil -> assert false
  | Cons (q, cv) -> lookup (Q.to_float q) cv

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
  let rec euclid p q = (* invariant 0 <= q < p *)
    if q = Z.zero then empty
    else let a, r = Z.div_rem p q in
         fun () -> Cons (a, euclid q r) in
  let q, r = Z.div_rem num den in
  fun () -> Cons (q, euclid den r)

let of_qstring s =
  of_q (Q.of_string s)

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

let of_ilist l = of_list (List.map Z.of_int l)

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

type bound = Inf | Fin of Z.t

let bound n d = if d = Z.zero then Inf else Fin (Z.fdiv n d)

let _print_bound fmt = function
  | Inf   -> Format.fprintf fmt "inf"
  | Fin z -> Z.pp_print fmt z

(* Given x, we wish to compute the homographic function

    a + bx
   --------
    c + dx
*)
let homography ?(a=Z.zero) ?(b=Z.zero) ?(c=Z.zero) ?(d=Z.zero) x =
  if c = Z.zero && d = Z.zero then invalid_arg "homography";
  let debug = false in
  let rec next a b c d x () =
    if debug then
      Format.eprintf "state is %a %a / %a %a@."
        Z.pp_print a Z.pp_print b Z.pp_print c Z.pp_print d;
    if c = Z.zero && d = Z.zero then
      Nil
    else match bound a c, bound b d with
    | Fin q, Fin q' when q = q' -> (* egest *)
        if debug then Format.eprintf "egest %a@." Z.pp_print q;
        Cons (q, next c d Z.(a - q * c) Z.(b - q * d) x)
    | _ -> (match x () with (* ingest *)
        | Nil ->
            next b b d d x ()
        | Cons (p, x) ->
            if debug then Format.eprintf "ingest %a@." Z.pp_print p;
            next b Z.(a + p * b) d Z.(c + p * d) x ()) in
  next a b c d x

let ihomography ?(a=0) ?(b=0) ?(c=0) ?(d=0) =
  homography ~a:(Z.of_int a) ~b:(Z.of_int b) ~c:(Z.of_int c) ~d:(Z.of_int d)

let zadd a x = homography ~a ~b:Z.one ~c:Z.one x
let iadd a x = ihomography ~a ~b:1 ~c:1 x
let zmul b x = homography ~b ~c:Z.one x
let imul b x = ihomography ~b ~c:1 x
let zdiv x c = homography ~b:Z.one ~c x
let idiv x c = ihomography ~b:1 ~c x
let inv x = ihomography ~a:1 ~d:1 x

let _idiff p q =
  if p = infinity || q = infinity then infinity else abs_float (p -. q)

(* Given x and y, we wish to compute

    a + bx + cy + dxy
   -------------------
    e + fx + gy + hxy

  that Gosper calls a ``bihomographic function''.

     <------x emits p---------

    ...   a+bp      b     a    |
           e+fp      f     e   |
                               |
          c+dp      d     c    |
           g+hp       h     g  |
                               |
                  b+dq   a+cq  |  y emits q
                   f+hq   e+gq |
                               |
                      ...      V

*)
let bihomography
  ?(a=Z.zero) ?(b=Z.zero) ?(c=Z.zero) ?(d=Z.zero)
  ?(e=Z.zero) ?(f=Z.zero) ?(g=Z.zero) ?(h=Z.zero) x y =
  if e = Z.zero && f = Z.zero && g = Z.zero && h = Z.zero then
    invalid_arg "bihomography";
  let debug = false in
  let rec next a b c d e f g h x y () =
    if debug then
      Format.eprintf "state is %a %a %a %a / %a %a %a %a@."
        Z.pp_print a Z.pp_print b Z.pp_print c Z.pp_print d
        Z.pp_print e Z.pp_print f Z.pp_print g Z.pp_print h;
    if e = Z.zero && f = Z.zero && g = Z.zero && h = Z.zero then
      Nil
    else match bound a e, bound c g, bound b f, bound d h with
    | Fin q, Fin q2, Fin q3, Fin q4 when q = q2 && q2 = q3 && q3 = q4 ->
        (* egest *)
        if debug then Format.eprintf "egest %a@." Z.pp_print q;
        Cons (q, next e         f         g         h
                      Z.(a-q*e) Z.(b-q*f) Z.(c-q*g) Z.(d-q*h) x y)
    | _ ->
        if Z.(f = zero && h = zero ||
              e = zero && g = zero ||
              abs (b*e*g - a*f*g) > abs (c*e*f - a*f*g)) then
          ingest_x a b c d e f g h x y
        else
          ingest_y a b c d e f g h x y
    and ingest_x a b c d e f g h x y = match x () with
      | Nil ->
          next b b d d
               f f h h x y ()
      | Cons (p, x) ->
          if debug then Format.eprintf "ingest x => %a@." Z.pp_print p;
          next b Z.(a+b*p) d Z.(c+d*p)
               f Z.(e+f*p) h Z.(g+h*p) x y ()
    and ingest_y a b c d e f g h x y = match y () with
      | Nil ->
          next c d c d
               g h g h x y ()
      | Cons (q, y) ->
          if debug then Format.eprintf "ingest y => %a@." Z.pp_print q;
          next c d Z.(a+c*q) Z.(b+d*q)
               g h Z.(e+g*q) Z.(f+h*q) x y ()
  in
  next a b c d e f g h x y

let ibihomography ?(a=0) ?(b=0) ?(c=0) ?(d=0) ?(e=0) ?(f=0) ?(g=0) ?(h=0) =
  bihomography
    ~a:(Z.of_int a) ~b:(Z.of_int b) ~c:(Z.of_int c) ~d:(Z.of_int d)
    ~e:(Z.of_int e) ~f:(Z.of_int f) ~g:(Z.of_int g) ~h:(Z.of_int h)

let add x y = ibihomography ~b:1 ~c:1         ~e:1          x y
let sub x y = ibihomography ~b:1 ~c:(-1)      ~e:1          x y
let mul x y = ibihomography              ~d:1 ~e:1          x y
let div x y = ibihomography ~b:1                       ~g:1 x y

(** {2 Some continued fractions} *)

let rec constant z () = Cons (z, constant z)

(* phi = [1; (1)] *)
let phi = constant Z.one

(* sqrt(2) = [1; (2)] *)
let sqrt2 () = Cons (Z.one, constant (Z.of_int 2))

(* sqrt(3) = [1; (1, 2)] *)
let sqrt3 =
  let l12 = [Z.one; Z.of_int 2] in periodic [Z.one] (fun _ -> l12)

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

(** {Semi-computable functions} *)

type 'a semi = Sure of 'a | CantDecide

let default_fuel = 20

let compare ?(fuel=default_fuel) x y =
  let rec cmp fuel even x y =
    if fuel = 0 then CantDecide else
    match x(), y () with
    | Nil, Nil -> Sure 0
    | Nil, Cons _ -> Sure (if even then 1 else -1)
    | Cons _, Nil -> Sure (if even then -1 else 1)
    | Cons (zx, x), Cons (zy, y) ->
        let c = Z.compare zx zy in
        if c <> 0 then Sure (if if even then c < 0 else c > 0 then -1 else 1)
        else cmp (fuel - 1) (not even) x y in
  cmp fuel true x y

let partial f = function CantDecide -> CantDecide | Sure x -> Sure (f x)

let equal ?(fuel=default_fuel) x y =
  partial ((=) 0) (compare ~fuel x y)

let is_rational ?(fuel=default_fuel) x =
  let rec lookup fuel x =
    if fuel = 0 then CantDecide else
    match x () with Nil -> Sure true | Cons (_, x) -> lookup (fuel - 1) x in
  lookup fuel x
