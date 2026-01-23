(* Values *)
let unused = 42
let used = 42
let internally_used = 42
let externally_used = 42

let () = (* use values *)
  ignore used;
  ignore internally_used

(* Methods *)
let immediate = object
  method unused = 42
  method used = 42
  method internally_used = 42
  method externally_used = 42
end

let () = (* use methods *)
  ignore immediate#used;
  ignore immediate#internally_used

(* Constructors *)
type constructors =
  | Unused
  | Used
  | Internally_used
  | Externally_used

type constr_with_eq = Unused
[@@deriving eq]

let () = (* use constructors *)
  ignore Used;
  ignore Internally_used

(* Record fields *)
type record = {
  unused : int;
  used : int;
  internally_used : int;
  externally_used : int;
}

type record_with_eq = {implicitly_used : int} [@@deriving eq]

let () = (* use record fields *)
  let r = {unused = 42; used = 42; internally_used = 42; externally_used = 42} in
  ignore r.used;
  ignore r.internally_used

(* Optional arguments *)
let exported_f ?never ?always ?internally ?externally () =
  ignore never;
  ignore always;
  ignore internally;
  ignore externally

let unexported_f ?never ?always ?sometimes () =
  ignore never;
  ignore always;
  ignore sometimes

let internally_used_f ?never ?always ?sometimes () =
  ignore never;
  ignore always;
  ignore sometimes

let externally_used_f ?never ?always ?sometimes () =
  ignore never;
  ignore always;
  ignore sometimes

let () = (* use optional arguments *)
  exported_f ~always:42 ~internally:42 ();
  unexported_f ~always:42 ~sometimes:42 ();
  unexported_f ~always:42 ();
  internally_used_f ~always:42 ~sometimes:42 ();
  internally_used_f ~always:42 ()

(* Stylistic issues *)
let _ =
  let unit_binding = () in
  let expect_opt_arg_in_arg (f : ?opt:'a -> unit -> unit) = f () in
  let () (* sequence *) = ignore expect_opt_arg_in_arg in
  let useless_binding = 42 in
  useless_binding
