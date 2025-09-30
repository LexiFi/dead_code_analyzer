type v = Used | Unused

type v2 = Int of int | Float of float | Nan | Nothing

let i = Int 0

let print_number = function
  | Int i -> print_int i
  | Float f -> print_float f
  | _ -> ()

let f: [`Value of int (* Should be detected ? *)| `Unit] -> v2 = function
  | `Value i -> Int i
  | `Unit -> Nan

let () =
  let a = f `Unit in
  match Used with
  | Used -> if a = i then print_number a
