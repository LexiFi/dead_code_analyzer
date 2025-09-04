type t = {
  success : int;
  fp : int;
  fn : int
}

let total t = t.success + t.fp + t.fn
let expected t = t.success + t.fn
let failed t = t.fp + t.fn

(* Useful for debug *)
let to_string t =
  Printf.sprintf
    "{success = %d; fp = %d; fn = %d}"
    t.success t.fp t.fn

let pp t =
  let module PP = Pretty_print in
  let total = total t in
  Printf.printf "Total: %s%d%s\n" PP.blue total PP.style_reset;
  Printf.printf "Success: %s%d%s\n" PP.green t.success PP.style_reset;
  Printf.printf "Failed: %s%d%s\n" PP.red (failed t) PP.style_reset;
  let ratio = 100. *. float_of_int t.success /. float_of_int total in
  let color =
    if ratio < 50. then PP.red
    else if ratio < 80. then PP.yellow
    else PP.green
  in
  Printf.printf "Ratio: %s%F%%%s\n%!" color ratio PP.style_reset

let init = {success = 0; fp = 0; fn = 0}

let incr_fp t = {t with fp = t.fp + 1}
let incr_fn t = {t with fn = t.fn + 1}
let incr_success t = {t with success = t.success + 1}
