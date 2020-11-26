
open Format
open Seq
open Cfrac

let limit = ref max_int

let rec loop n cf =
  if n mod 1000 = 0 then printf "@?";
  if n < !limit then
  match cf () with
  | Nil -> assert false
  | Cons (z, cf) -> printf "%a,@ " Z.pp_print z; loop (n+1) cf

let () =
  loop 0 (terms pi)
