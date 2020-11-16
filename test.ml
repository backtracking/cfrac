
open Format
open Cfrac

let () = set_print_precision 10

let rec print_convergents n fmt cv = match cv () with
  | Seq.Nil -> ()
  | Seq.Cons _ when n = 0 -> fprintf fmt "..."
  | Seq.Cons (Q.{ num; den }, cv) ->
      fprintf fmt "%a/%a,@ %a" Z.pp_print num Z.pp_print den
        (print_convergents (n - 1)) cv

let print_convergents ~n fmt cv =
  fprintf fmt "@[<hov 2>%a@]" (print_convergents n) (convergents cv)

let display ?(n=10) name cf =
  printf "%s = %a@." name print cf;
  printf "  = %a@." (print_convergents ~n) cf;
  printf "@."

let () = display "0" zero
let () = display "1" one

let () = display "pi" pi
let () = display "phi" phi
let () = assert (nth_convergent 0 pi = Q.of_int 3)
let () = assert (nth_convergent 1 pi = Q.of_ints 22 7)
let () = assert (nth_convergent 3 pi = Q.of_ints 355 113)

let cf_42 = of_int 42
let () = assert (int_part cf_42 = Z.of_int 42)

let () = display "22/7" (of_qstring "22/7")
let () = display "355/113" (of_qstring "355/113")
let () = display "355/113 alt" (of_ilist [3; 7; 15; 1])
let () = display "103993/33102" (of_qstring "103993/33102")
let () = display "1+22/7" (iadd 1 (of_qstring "22/7"))

let () = assert (nth_convergent 1 (of_float 0.5) = Q.of_string "1/2")

let () = display "e" e
let () =
  let q = nth_convergent 14 e in
  printf "e ~ %a = %.10f@.@." Q.pp_print q (Q.to_float q)

let () =
  display "tan(1)" (periodic [Z.one] (fun n -> [Z.of_int (2*n+1); Z.one]))

let () =
  let x = of_q (Q.of_string "13/11") in
  display "13/11" x;
  display "x+1/2" (ihomography ~a:1 ~b:2 ~c:2 x);
  display "13/11 * 11" (imul 11 x);
  display "13/11 / 13" (idiv x 13)

let () = display "sqrt(2)" sqrt2
let () = display "1/sqrt(2)" (inv sqrt2)

let () =
  printf "approx pi 10-6 = %a@.@."
    Q.pp_print (best_approx (Z.of_int 1_000_000) pi)

let () = display "1*1" (mul one one)

let () = display "355/113 * 113/355" (mul (of_qstring "355/113") (of_qstring "113/355"))

let () = display "phi^2" (mul phi phi)
let () = display "phi+1" (iadd 1 phi)

let () = assert (compare (of_qstring "1/7") (of_qstring "1/5") = Sure (-1))
let () = assert (compare (of_qstring "1/5") (of_qstring "1/5") = Sure 0)

let () = assert (compare sqrt2 (of_qstring "15/10") = Sure (-1))
let () = assert (compare (of_qstring "15/10") sqrt2 = Sure 1)
let () = assert (compare sqrt2 (of_qstring "14/10") = Sure 1)
let () = assert (compare (of_qstring "14/10") sqrt2 = Sure (-1))
let () = assert (compare phi phi = CantDecide)

(* Examples from "Continued Fraction Arithmetic" by Bill Gosper
   See for instance https://perl.plover.com/classes/cftalk/INFO/gosper.txt *)

let () =
  display "number of inches per meter" (of_q (Q.of_string "10000/254"))
let cf = of_fun (function
             | 0 -> Z.of_int 39
             | 1 -> Z.of_int 2
             | 2 -> Z.of_int 1
             | 3 -> Z.of_int 2
             | 4 -> Z.of_int 2
             | 5 -> Z.of_int 1
             | 6 -> Z.of_int 4
             | _ -> Z.zero)
let () = display "" cf
let () = display "" (of_ilist [39; 2; 1; 2; 2; 1; 4])

let () =
  let cf = ihomography ~a:1 ~b:1 ~c:(-1) ~d:1 e in
  display "coth(1/2) = (e+1)/(e-1)" cf
