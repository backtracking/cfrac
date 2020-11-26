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

let debug = false
let _DEBUG f =
  if debug then Format.eprintf f else Format.ifprintf Format.err_formatter f

type t = Z.t Seq.t
(* this is a nonempty sequence (the first term is the floor) *)

let terms cf = cf

let first s = match s () with Nil -> assert false | Cons (x, y) -> x, y
let floor cf = fst (first cf)

let convergents cf =
  let rec seq hn_2 hn_1 kn_2 kn_1 cf () = match cf () with
    | Nil -> Nil
    | Cons (an, cf) ->
        let hn = Z.(an * hn_1 + hn_2) in
        let kn = Z.(an * kn_1 + kn_2) in
        (* we know that kn>0 and that hn,kn are relatively prime
           so we can safely skip Q.make *)
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
  let q, cv = first (convergents x) in
  lookup q cv

let two = Z.of_int 2

(* Conversion to a float (algorithm by Guillaume Melquiond):
   1. find a lower bound M of x
   2. compute a convergent q of x with a denominator at least ceil(2^30/sqrt(M))
      round this to the nearest float f (using Q.to_float)
   3. keep converting the next convergents to floats, until we get twice the
      same floating point number *)
let to_float x =
  let rec phase2 f cv = match cv () with
    | Nil -> f
    | Cons (q, cv) ->
        let f' = Q.to_float q in if f' = f then f else phase2 f' cv in
  let rec phase1 d q cv = match cv () with
    | Nil -> Q.to_float q
    | Cons (q, cv) when q.Q.den >= d -> phase2 (Q.to_float q) cv
    | Cons (q, cv) -> phase1 d q cv in
  let start d cv = match cv () with
    | Nil -> assert false
    | Cons (q, cv) when q.Q.den >= d -> phase2 (Q.to_float q) cv
    | Cons (q, cv) -> phase1 d q cv in
  let t30 = Z.(pow two 30) in
  let a0, cf = first x in
  if a0 = Z.zero then
    match cf () with
    | Nil -> 0.
    | Cons (a1, _) -> assert (Z.sign a1 > 0);
                      start (Z.mul t30 (Z.sqrt a1)) (convergents x)
  else
    start (Z.cdiv t30 Z.(sqrt (abs a0))) (convergents x)

let print ~prec fmt x =
  let rec print n fmt a = match a () with
    | Nil -> ()
    | Cons _ when n = 0 -> Format.fprintf fmt "..."
    | Cons (an, a) ->
        Format.fprintf fmt "%a,@ %a" Z.pp_print an (print (n-1)) a in
  let a0, a = first x in
  Format.fprintf fmt "[@[<hov 2> %a;@ %a@]]" Z.pp_print a0 (print prec) a

let print_convergents ~prec fmt x =
  let rec print_convergents n fmt cv = match cv () with
  | Seq.Nil -> ()
  | Seq.Cons _ when n = 0 -> Format.fprintf fmt "..."
  | Seq.Cons (Q.{ num; den }, cv) ->
      Format.fprintf fmt "%a/%a,@ %a" Z.pp_print num Z.pp_print den
        (print_convergents (n - 1)) cv in
  Format.fprintf fmt "@[<hov 2>%a@]" (print_convergents prec) (convergents x)

let ten = Z.of_int 10

(* Print up to n decimals of the rational number a/b.
   Print the period when possible i.e. n is large enough. *)
let print_rat n fmt a b = (* invariant 0 <= a/b < 10 *)
  let buf = Buffer.create 1024 in
  let index = Hashtbl.create 256 in
  let rec loop n i a b = (* invariant |buf|=i *)
    if a = Z.zero then
      Format.fprintf fmt "%s" (Buffer.contents buf)
    else if n = 0 then
      Format.fprintf fmt "%s..." (Buffer.contents buf)
    else match Hashtbl.find_opt index a with
    | None ->
        Hashtbl.add index a i;
        let q, r = Z.div_rem a b in
        assert (q < ten);
        Buffer.add_char buf (Char.chr (48 + Z.to_int q));
        loop (n-1) (i+1) Z.(ten*r) b
    | Some p ->
        Format.fprintf fmt "%s(%s)*"
         (Buffer.sub buf 0 p) (Buffer.sub buf p (i - p))
  in
  loop n 0 a b

let print_decimals ~prec fmt x =
  let rec print n a b c d fmt cf =
    (* invariant 0 <= a/b, c/d <= 10 *)
    (* Note: we would like to say instead 0 < a/b, c/d < 10 but this is actually
       not the case at the very first step, that is 0/1 10/a.
       Indeed, the first term a may be 1. But then it cannot be the last term
       (which cannot be one), so there will be another step and the invariant
       will be established. *)
    _DEBUG "  %d %a/%a %a/%a@." n Z.pp_print a
        Z.pp_print b Z.pp_print c Z.pp_print d;
    if n = 0 then
      match cf () with Nil when c = Z.zero -> () | _ -> Format.fprintf fmt "..."
    else (
      let z = Z.fdiv a b in
      let z' = Z.fdiv c d in
      assert Z.(zero <= z && z <= ten);
      assert Z.(zero <= z' && z' <= ten);
      if z = z' then ( (* we have a digit *)
        Format.fprintf fmt "%a@," Z.pp_print z;
        print (n-1) Z.(ten*(a-b*z)) b Z.(ten*(c-d*z)) d fmt cf
        (* FIXME: do we need to simplify ten*.../d ? *)
      ) else match cf () with
      | Nil ->
          _DEBUG "[STOP %a/%a %a/%a]" Z.pp_print a
            Z.pp_print b Z.pp_print c Z.pp_print d;
          print_rat n fmt c d
      | Cons (z, cf) ->
          print n c d Z.(z * c + a) Z.(z * d + b) fmt cf
    ) in
  let z, x = first x in
  match x () with
  | Nil -> Z.pp_print fmt z
  | Cons (a, x) ->
      assert Z.(z >= zero);
      Format.fprintf fmt "@[<hov 2>%a.%a@]"
        Z.pp_print z (print prec Z.zero Z.one ten a) x
      (* FIXME: do we need to simplify 10/a ? *)

(** {2 constructors} *)

let of_int n =
  return (Z.of_int n)

let zero = of_int 0
let one  = of_int 1

let of_z z =
  return z

let of_q Q.{ num; den } =
  assert (Z.sign den > 0);
  let rec euclid p q = (* invariant 0 <= q < p *)
    if q = Z.zero then empty
    else let a, r = Z.div_rem p q in
         fun () -> Cons (a, euclid q r) in
  let q, r = Z.ediv_rem num den in
  fun () -> Cons (q, euclid den r)

let iinv n =
  of_q (Q.make Z.one (Z.of_int n))

let of_qstring s =
  of_q (Q.of_string s)

let of_float x =
  of_q (Q.of_float x)

let of_seq s =
  let check z = if Z.sign z <= 0 then invalid_arg "of_seq"; z in
  match s () with
  | Nil -> invalid_arg "of_seq"
  | Cons (z, s) -> fun () -> Cons (z, Seq.map check s)

let of_fun f =
  let rec loop i () =
    let z = f i in
    if Z.sign z < 0 then invalid_arg "of_fun";
    if Z.sign z = 0 then Nil else Cons (z, loop (i + 1)) in
  fun () ->
    let z = f 0 in
    Cons (z, loop 1)

let rec seq_of_list l () = match l with
  | [] -> Nil
  | z :: _ when Z.sign z <= 0 -> invalid_arg "of_list"
  | z :: l -> Cons (z, seq_of_list l)

let of_list = function
  | [] -> invalid_arg "of_list"
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
  | z :: l -> fun () -> Cons (z, list_concat l (list_flatten f 0))

let rec concat s1 s2 () = match s1 () with
  | Nil -> s2 ()
  | Cons (z, s) -> Cons (z, concat s s2)

let rec repeat s () = concat s (repeat s) ()

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
  let rec next a b c d x () =
    _DEBUG "state is %a %a / %a %a@."
        Z.pp_print a Z.pp_print b Z.pp_print c Z.pp_print d;
    if c = Z.zero && d = Z.zero then
      Nil
    else match bound a c, bound b d with
    | Fin q, Fin q' when q = q' -> (* egest *)
        _DEBUG "egest %a@." Z.pp_print q;
        Cons (q, next c d Z.(a - q * c) Z.(b - q * d) x)
    | _ -> (match x () with (* ingest *)
        | Nil ->
            next b b d d x ()
        | Cons (p, x) ->
            _DEBUG "ingest %a@." Z.pp_print p;
            next b Z.(a + p * b) d Z.(c + p * d) x ()) in
  next a b c d x

let ihomography ?(a=0) ?(b=0) ?(c=0) ?(d=0) =
  homography ~a:(Z.of_int a) ~b:(Z.of_int b) ~c:(Z.of_int c) ~d:(Z.of_int d)

let zadd a x = homography ~a ~b:Z.one ~c:Z.one x
let iadd a x = ihomography ~a ~b:1 ~c:1 x
let zmul b x = homography ~b ~c:Z.one x
let imul b x = ihomography ~b ~c:1 x
let neg x = ihomography ~b:(-1) ~c:1 x
let zdiv x c = homography ~b:Z.one ~c x
let idiv x c = ihomography ~b:1 ~c x

(* note: [inv] could be derived from [homography] but it has a much simpler
   formulation *)
let inv x =
  let z0, x' = first x in
  if z0 = Z.zero then x' else fun () -> Cons (Z.zero, x)

let print_decimals ~prec fmt x =
  let z, x' = first x in
  match x' () with
  | Nil -> Z.pp_print fmt z
  | Cons _ when z >= Z.zero -> print_decimals ~prec fmt x
  | Cons _ ->
      (* z + 1/x' = -(-z-1 + 1-1/x') = -(-z-1 + 1/(x/(x-1))) *)
      let x () = Cons (Z.(-z - one), ihomography ~a:0 ~b:1 ~c:(-1) ~d:1 x') in
      Format.fprintf fmt "-%a" (print_decimals ~prec) x

(* Given x and y, we wish to compute

    a + bx + cy + dxy
   -------------------
    e + fx + gy + hxy

  that Gosper calls a ``bihomographic function''.

     +------------------x emits p--------->
     |
     |        a      b     a+bp    ...
     |         e      f     e+fp
     |
     |        c      d     c+dp    ...
     |         g      h     g+hp
 y emits q
     |        a+cq   b+dq
     |         e+gq   f+hq      .
     |                            .
     V        ...    ...            .

*)
let bihomography
  ?(a=Z.zero) ?(b=Z.zero) ?(c=Z.zero) ?(d=Z.zero)
  ?(e=Z.zero) ?(f=Z.zero) ?(g=Z.zero) ?(h=Z.zero) x y =
  if e = Z.zero && f = Z.zero && g = Z.zero && h = Z.zero then
    invalid_arg "bihomography";
  let rec next a b c d e f g h x y () =
    _DEBUG "state is %a %a %a %a / %a %a %a %a@."
      Z.pp_print a Z.pp_print b Z.pp_print c Z.pp_print d
      Z.pp_print e Z.pp_print f Z.pp_print g Z.pp_print h;
    if e = Z.zero && f = Z.zero && g = Z.zero && h = Z.zero then
      Nil
    else match bound a e, bound c g, bound b f, bound d h with
    | Fin q, Fin q2, Fin q3, Fin q4 when q = q2 && q2 = q3 && q3 = q4 ->
        (* egest *)
        _DEBUG "egest %a@." Z.pp_print q;
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
          _DEBUG "ingest x => %a@." Z.pp_print p;
          next b Z.(a+b*p) d Z.(c+d*p)
               f Z.(e+f*p) h Z.(g+h*p) x y ()
    and ingest_y a b c d e f g h x y = match y () with
      | Nil ->
          next c d c d
               g h g h x y ()
      | Cons (q, y) ->
          _DEBUG "ingest y => %a@." Z.pp_print q;
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
let sqr x = mul x x

type 'a memo = Done of 'a | Todo of (unit -> 'a)

let rec memo cf =
  let r = ref (Todo cf) in
  (fun () -> match !r with
             | Todo f -> (match f () with
                          | Nil -> r := Done Nil; Nil
                          | Cons (x, cf) -> let v = Cons (x, memo cf) in
                                            r := Done v; v)
             | Done x -> x)

(* Convert a generalized CF (with arbitrary numerators) to a simple CF.
   In the meantime, we apply the homographic function

    a + bx
   --------
    c + dx
*)
let generalized ?(a=Z.zero) ?(b=Z.one) ?(c=Z.one) ?(d=Z.zero) g =
  let rec loop pold qold p q num den g () =
    _DEBUG "LOOP %a/%a %a/%a %a/%a@."
      Z.pp_print pold Z.pp_print qold Z.pp_print p Z.pp_print q
      Z.pp_print num Z.pp_print den;
    let pold, qold, p, q =
      p, q, Z.(pold*num + p*den), Z.(qold*num + q*den) in
    let pold, qold, p, q = Z.(
      let t = gcd (gcd p q) (gcd pold qold) in
      if t <> one then
        divexact pold t, divexact qold t, divexact p t, divexact q t
      else
        pold, qold, p, q) in
    _DEBUG "LOOP %a/%a %a/%a@."
      Z.pp_print pold Z.pp_print qold Z.pp_print p Z.pp_print q;
    if qold = Z.zero then
      input pold qold p q g
    else
      let t1, t0 = Z.div_rem pold qold in
      let t2 = Z.(q * t1) in
      if t2 <= p then
        let t2 = Z.(t2 + q) in
        if t2 > p then
          let t2 = Z.(p + q - t2) in
          Cons (t1, fun () -> input qold t0 q t2 g)
        else
          input pold qold p q g
      else
        input pold qold p q g
    and input pold qold p q g = match g () with
      | Nil ->
          of_q (Q.make p q) ()
      | Cons (num, g) ->
          let den, g = first g in
          loop pold qold p q num den g ()
  in
  match g () with
  | Nil -> invalid_arg "generalized"
  | Cons (den, g) -> loop a b c d Z.one den g

(** {2 Some continued fractions} *)

let rec constant z () = Cons (z, constant z)

(* phi = [1; (1)] *)
let phi = constant Z.one

(* sqrt(2) = [1; (2)] *)
let sqrt2 () = Cons (Z.one, constant two)

(* sqrt(3) = [1; (1, 2)] *)
let sqrt3 =
  let l12 = [Z.one; two] in periodic [Z.one] (fun _ -> l12)

(* using 4/pi = 1 + 1/(3 + 4/(5 + 9/(7 + 16/(9 + ...)))) *)
let pi =
  let rec gen n d () =
    Cons (d, fun () ->
    Cons (n, let d = Z.(d + two) in gen Z.(n + d) d)) in
  generalized ~a:(Z.of_int 4) ~b:Z.zero ~c:Z.zero ~d:Z.one (gen Z.one Z.one)

let pi = memo pi

(* e = [2; 1,2,1, 1,4,1, 1,6,1, 1,8,1, ...] = [2; (1, 2n+2, 1)] *)
let e = periodic [two] (fun n -> [Z.one; Z.of_int (2*n+2); Z.one])

let tan1 = periodic [Z.one; Z.one] (fun n -> [Z.one; Z.of_int (2*n+3)])

let tan_iinv n =
  if n <= 1 then invalid_arg "tan_iinv";
  periodic [Z.zero; Z.of_int (n-1)] (fun k -> [Z.one; Z.of_int ((2*k+3)*n-2)])

let exp_iinv n =
  if n <= 1 then invalid_arg "exp_iinv";
  periodic [Z.one; Z.of_int (n-1)]
    (fun k -> [Z.one; Z.one; Z.of_int ((2*k+3)*n-1)])

(* Source: https://www.cs.jhu.edu/~jason/software/fractions/ *)
let sqrt_z d =
  if Z.(d <= one) then invalid_arg "sqrt_z";
  let sd = Z.sqrt d in
  if d = Z.mul sd sd then invalid_arg "sqrt_z";
  let target = Z.mul two sd in
  let rec loop l a b c = (* (a + b sqrt(d)) / c *)
    let bbd = Z.(b * b * d) in
    let q = Z.(fdiv (a + sqrt bbd) c) in
    let a = Z.(a - c*q) in
    let a, b, c = Z.(-a*c, b*c, bbd - a*a) in
    let g = Z.gcd (Z.gcd a b) c in
    let a, b, c = Z.divexact a g, Z.divexact b g, Z.divexact c g in
    let l = q :: l in
    if q = target then List.to_seq (List.rev l), List.length l
    else loop l a b c
  in
  let s, prec = loop [] sd Z.one Z.(d - sd*sd) in
  (fun () -> Cons (sd, repeat s)), prec

let sqrt_int d =
  sqrt_z (Z.of_int d)

let pell_fermat ?(neg=false) n =
  let cf, m =
    try sqrt_z n with Invalid_argument _ -> invalid_arg "pell_fermat" in
  let t = if neg then Z.minus_one else Z.one in
  let f Q.{num=x; den=y} =
    if Z.(x*x - n*y*y = t) then Some (x, y) else None in
  if neg && m land 1 = 0 then empty else Seq.filter_map f (convergents cf)

let pell_fermat_int ?neg n =
  pell_fermat ?neg (Z.of_int n)

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
