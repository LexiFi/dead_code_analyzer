type t = {
  success : int;
  fp : int;
  fn : int;
  other_failures : int
}

let total t = t.success + t.fp + t.fn + t.other_failures
let expected t = t.success + t.fn
let failed t = t.fp + t.fn + t.other_failures

(* Useful for debug *)
let to_string t =
  Printf.sprintf
    "{success = %d; fp = %d; fn = %d}"
    t.success t.fp t.fn

module PP = Pretty_print

let make_fmt title color =
  let line =
    Printf.sprintf "%s: %s%%d%s" title color PP.style_reset
  in
  Scanf.format_from_string line "%d"

let total_fmt () = make_fmt "Total" PP.blue
let success_fmt () = make_fmt "Success" PP.green
let failed_fmt () = make_fmt "Failed" PP.red

let extract_from fmt s =
  try Scanf.sscanf s fmt Option.some
  with _ -> None

let extract_total = extract_from (total_fmt ())
let extract_success = extract_from (success_fmt ())
let extract_failed = extract_from (failed_fmt ())

let pp t =
  let total = total t in
  let print_line fmt value =
    Printf.printf (fmt ()) value;
    Printf.printf "\n"
  in
  print_line total_fmt total;
  print_line success_fmt t.success;
  print_line failed_fmt (failed t);
  let ratio = 100. *. float_of_int t.success /. float_of_int total in
  let color =
    if ratio < 50. then PP.red
    else if ratio < 80. then PP.yellow
    else PP.green
  in
  Printf.printf "Ratio: %s%F%%%s\n%!" color ratio PP.style_reset

let init = {success = 0; fp = 0; fn = 0; other_failures = 0}

let incr_fp t = {t with fp = t.fp + 1}
let incr_fn t = {t with fn = t.fn + 1}
let incr_success t = {t with success = t.success + 1}

let set_success success t = {t with success}
let set_failures other_failures t = {t with other_failures}
