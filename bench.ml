
open Cfrac

let () =
  for _ = 1 to 1_000_000 do
    ignore (to_float phi)
  done

