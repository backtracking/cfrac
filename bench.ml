
open Format
open Cfrac
open Unix

let start, stop =
  let t = ref 0. in
  (fun () -> t := (times()).tms_utime),
  (fun () -> printf "=> %f s@." ((times()).tms_utime -. !t))

let test ?(n=1_000_000) msg x =
  printf "%s:@." msg;
  printf "  to_float: @?";
  start ();
  for _ = 1 to n do ignore (to_float x) done;
  stop ();
  (* printf "  to_float2: @?";
   * start ();
   * for _ = 1 to n do ignore (to_float2 x) done;
   * stop ();
   * printf "  to_float12: @?";
   * start ();
   * for _ = 1 to n do ignore (to_float12 x) done;
   * stop (); *)
  ()

let () =
  test "phi" phi;
  test "pi" pi;
  test "e" e;
  test "sqrt(2)" sqrt2;
  test "sqrt(3)" sqrt3;
  test "1/phi" (memo (inv phi));
  printf "10000 decimals of pi: @?";
  start ();
  let b = Buffer.create 10000 in
  let fmt = formatter_of_buffer b in
  ignore (fprintf fmt "%a@." (print_decimals ~prec:10000) pi);
  stop ();
  ()
