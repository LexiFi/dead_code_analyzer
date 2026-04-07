(* extensible_variant_bin.ml *)
open Extensible_variant_lib
type t += String of string

let to_string_opt = function
  | String s -> Some s
  | t -> to_string_opt t

let () =
  match to_string_opt (Int 42) with
  | Some s -> print_endline s
  | None -> ()

