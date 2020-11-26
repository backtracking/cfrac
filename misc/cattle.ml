
(* Archimedes's cattle problem
   See https://en.wikipedia.org/wiki/Archimedes%27s_cattle_problem *)

open Format
open Cfrac

let s = pell_fermat_int (4 * 609 * 7766 * 4657 * 4657)
let q = match s () with Nil -> assert false | Cons ((_,q), _) -> q

let () =
  printf "q = %a@." Z.pp_print q;
  printf "k =  4 456 749 * q^2@.";
  printf "W =  7 460 514 * k white bulls@.";
  printf "B = 10 366 482 * k black bulls@.";
  printf "D =  7 358 060 * k dappled bulls@.";
  printf "Y =  4 149 387 * k yellow bulls@.";
  printf "w =  4 893 246 * k white cows @.";
  printf "b =  7 206 360 * k black cows@.";
  printf "d =  3 515 820 * k dappled cows@.";
  printf "y =  5 439 213 * k yellow cows@.";

open Z

let total =
  let k = of_int 4_456_749 * q * q in
  (of_int  7_460_514 +
   of_int 10_366_482 +
   of_int  7_358_060 +
   of_int  4_149_387 +
   of_int  4_893_246 +
   of_int  7_206_360 +
   of_int  3_515_820 +
   of_int  5_439_213 ) * k

let () =
  printf "Total number of cattle in the herd is@.";
  printf "%a@." Z.pp_print total

(* After 3 minutes of computation, we get

q = 1858921901...0208663490 (103266 digits)
...
Total number of cattle in the herd is
7760271406...9455081800 (206545 digits)

See enclosed file cattle.out. *)
