
open Format
open Cfrac
open Unix

let start, stop =
  let t = ref 0. in
  (fun () -> t := (times()).tms_utime),
  (fun fmt () -> fprintf fmt "%f" ((times()).tms_utime -. !t))

let test ?(n=1_000_000) msg x =
  assert (to_float1 x = to_float2 x);
  printf "%s:@." msg;
  start ();
  for _ = 1 to n do
    ignore (to_float1 x)
  done;
  printf "  to_float1: %a@." stop ();
  start ();
  for _ = 1 to n do
    ignore (to_float2 x)
  done;
  printf "  to_float2: %a@." stop ()

let () =
  test "phi" phi;
  test "pi"  pi;
  test "sqrt(2)" sqrt2;
  test "sqrt(3)" sqrt3;
  test "1/phi" (memo (inv phi));
  ()
