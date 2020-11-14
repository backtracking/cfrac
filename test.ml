
open Format
open Cfrac

let rec print_convergents n fmt cv = match cv () with
  | Seq.Nil -> ()
  | Seq.Cons _ when n = 0 -> fprintf fmt "..."
  | Seq.Cons (Q.{ num; den }, cv) ->
      fprintf fmt "%a/%a,@ %a" Z.pp_print num Z.pp_print den
        (print_convergents (n - 1)) cv

let print_convergents ~n fmt cv =
  fprintf fmt "@[<hov 2>%a@]" (print_convergents n) (convergents cv)

let () = printf "phi = %a@." print phi
let () = set_print_precision 10
let () = printf "phi = %a@." print phi
let () = printf "phi = %a@." (print_convergents ~n:10) phi
let () = printf "@."

let () = printf "pi  = %a@." print pi
let () = printf "pi  = %a@." (print_convergents ~n:5) pi
let () = printf "@."
