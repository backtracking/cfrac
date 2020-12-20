
open Format
open Cfrac

let display ?(n=10) ?(decimals=30) name cf =
  printf "%s@.  = %a@." name (print ~prec:n) cf;
  printf "  = %a@." (print_convergents ~prec:10) cf;
  printf "  = %a@." (print_decimals ~prec:decimals) cf;
  printf "  ~ %.15f@." (to_float cf);
  printf "@."

let displayq s = display s (of_qstring s)

let () = display "0" zero
let () = display "1" one

let () = display ~n:100 ~decimals:1000 "pi" pi
let () =
  printf "approx pi 10-6 = %a@.@."
    Q.pp_print (best_approx (Z.of_int 1_000_000) pi)

let () = display ~decimals:100 "phi" phi
let () = assert (to_float phi = (1. +. sqrt 5.) /. 2.)
let () = assert (nth_convergent 0 pi = Q.of_int 3)
let () = assert (nth_convergent 1 pi = Q.of_ints 22 7)
let () = assert (nth_convergent 3 pi = Q.of_ints 355 113)
let () = display "phi^2" (mul phi phi)
let () = display "phi+1" (iadd 1 phi)

let cf_42 = of_int 42
let () = assert (floor cf_42 = Z.of_int 42)

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
  display "13/11+1/2" (ihomography ~a:1 ~b:2 ~c:2 x);
  display "13/11 * 11" (imul 11 x);
  display "13/11 / 13" (idiv x 13)

let () = display "sqrt(2)" sqrt2
let () = display "1/sqrt(2)" (inv sqrt2)
let () = display "sqrt(2)/2" (idiv sqrt2 2)
let () = assert (to_float sqrt2 = sqrt 2.)

let () = display "sqrt(3)" sqrt3

let () = display "1*1" (mul one one)

let () = display "355/113 * 113/355" (mul (of_qstring "355/113") (of_qstring "113/355"))

let () = assert (compare (of_qstring "1/7") (of_qstring "1/5") = Sure (-1))
let () = assert (compare (of_qstring "1/5") (of_qstring "1/5") = Sure 0)

let () = assert (compare sqrt2 (of_qstring "15/10") = Sure (-1))
let () = assert (compare (of_qstring "15/10") sqrt2 = Sure 1)
let () = assert (compare sqrt2 (of_qstring "14/10") = Sure 1)
let () = assert (compare (of_qstring "14/10") sqrt2 = Sure (-1))
let () = assert (compare phi phi = CantDecide)

let () = assert (to_float (of_qstring "1/2") = 0.5)
let () = printf "0.01 ~ %.15f@.@." (to_float (of_qstring "1/100"))
let () = let av = 6.02214076e+23 in assert (to_float (of_float av) = av)
let () = assert (to_float (of_float 1e-200) = 1e-200)
let () = assert (to_float (of_float 1e+300) = 1e+300)

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

let () = displayq "1/7"

let () =
  for n = 2 to 31 do
    printf "1/%2d = %a@." n (print_decimals ~prec:60) (iinv n)
  done;
  printf "@."

let () = display "pi^2/6" (ibihomography ~d:1 pi pi ~e:6)

let () = displayq "-143/12"

let () = display "tan(1)" tan1
let () = display "tan(1/2)" (tan_iinv 2)
let () = display "tan(1/1000)" (tan_iinv 1000)
let () = display "exp(1/2)" (exp_iinv 2)
let () = display "exp(1/2)^2" (let x = memo (exp_iinv 2) in mul x x)

let () = display "-1/7" (iinv (-7))

let () =
  let maxp = ref 0 in
  let maxi = ref 0 in
  for i = 2 to 167 do
    let d = Z.of_int i in
    try
      let cf, prec = sqrt_z d in
      if prec > !maxp then (maxp := prec; maxi := i);
      let prec = min prec 15 in
      printf "sqrt(%3d) = %a@." i (print ~prec) cf
    with Invalid_argument _ -> ()
  done;
  printf "max period %d for d=%d@." !maxp !maxi

let () =
  let pf neg =
    printf "Pell-Fermat equation x^2-dy^2 = %d@." (if neg then -1 else 1);
    for d = 2 to 167 do
      try
        let s = pell_fermat_int ~neg d in
        printf "  d = %3d: @[" d;
        let rec print fuel s =
          if fuel = 0 then printf "...@]@." else match s () with
          | Seq.Nil -> printf "no solutions@]@."
          | Cons ((x,y), s) -> printf "%a,%a /@ " Z.pp_print x
                             Z.pp_print y; print (fuel-1) s in
        print 5 s
      with Invalid_argument _ -> ()
    done in
  pf false;
  pf true

let test_interval msg x =
  printf "intervals for %s@.  = %a@." msg (print ~prec:10) x;
  let print n =
    let prec = { Q.num = Z.one; den = Z.of_int n } in
    let lo, hi = interval prec x in
    let delta = Q.(to_float (hi - lo)) in
    assert (delta < 1. /. float n);
    printf "  = %a..%a = %f..%f (delta = %f < 1/%d)@."
      Q.pp_print lo Q.pp_print hi (Q.to_float lo) (Q.to_float hi) delta n in
  print 1;
  print 1_000;
  print 1_000_000

let () = test_interval "1" one
let () = test_interval "1/7" (iinv 7)
let () = test_interval "pi" pi
let () = test_interval "sqrt(2)" sqrt2

let () =
  let x = ref e in
  for n = 1 to 50 do x := imul n (iadd (-1) !x) done;
  display "after 50 years, I'll get" (iadd (-1) !x)



