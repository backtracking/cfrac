
open Seq

type t = Z.t Seq.t

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

(** {2 Some continued fractions} *)

(* phi = [1; (1)] *)
let rec phi () = Cons (Z.one, phi)

let pi =
  let a001203 =
    [3; 7; 15; 1; 292; 1; 1; 1; 2; 1; 3; 1; 14; 2; 1; 1; 2; 2; 2; 2; 1; 84;
        2; 1; 1; 15; 3; 13; 1; 4; 2; 6; 6; 99; 1; 2; 2; 6; 3; 5; 1; 1; 6; 8;
        1; 7; 1; 2; 3; 7; 1; 2; 1; 1; 12; 1; 1; 1; 3; 1; 1; 8; 1; 1; 2; 1; 6;
        1; 1; 5; 2; 2; 3; 1; 2; 4; 4; 16; 1; 161; 45; 1; 22; 1; 2; 2; 1; 4;
        1; 2; 24; 1; 2; 1; 3; 1; 2; 1] in
  let rec cf = function
    | [] -> failwith "precision of pi exceeded"
    | ai :: a -> fun () -> Cons (Z.of_int ai, cf a) in
  cf a001203

