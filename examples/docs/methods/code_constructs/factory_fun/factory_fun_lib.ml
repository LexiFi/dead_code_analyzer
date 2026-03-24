(* factoy_fun_lib.ml *)
let stack = ref []

let get_stack () =
  object
    method push x = stack := x::!stack
    method pop =
      match !stack with
      | [] -> ()
      | _::tl -> stack := tl
    method peek =
      match !stack with
      | [] -> None
      | hd::_ -> Some hd
    method reset = stack := []
  end
