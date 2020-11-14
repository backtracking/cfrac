
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


let display name cf =
  printf "%s = %a@." name print cf;
  printf "  = %a@." (print_convergents ~n:10) cf;
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

let () = display "q1"
           (of_q Q.{ num = Z.of_string "22"; den = Z.of_string "7" })
let () = display "q2"
           (of_q Q.{ num = Z.of_string "103993"; den = Z.of_string "33102" })
