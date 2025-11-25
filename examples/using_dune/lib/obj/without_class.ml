let immediate = object
  val unused_int_instance_var = 42
  val used_int_instance_var = 42
  method unused_int = used_int_instance_var
  method used_int = 42
  method internally_used_int = 42
  method externally_used_int = 42
  method unused_fun () = ()
  method used_fun () = ()
  method internally_used_fun () = ()
  method externally_used_fun () = ()
end

let factory () = object
  val unused_int_instance_var = 42
  val used_int_instance_var = 42
  method unused_int = used_int_instance_var
  method used_int = 42
  method internally_used_int = 42
  method externally_used_int = 42
  method unused_fun () = ()
  method used_fun () = ()
  method internally_used_fun () = ()
  method externally_used_fun () = ()
end

let unexported_immediate = object
  method unused_int = 42
  method used_int = 42
  method unused_fun () = ()
  method used_fun () = ()
end

let unexported_factory () = object
  method unused_int = 42
  method used_int = 42
  method unused_fun () = ()
  method used_fun () = ()
end

(* unused object *)
let _self_used_immediate = object (self)
  method unused_int = self#used_int
  method used_int = ()
  method unused_fun () = self#used_fun ()
  method used_fun () = ()
end

(* unused function *)
let _self_used_factory () = object (self)
  method unused_int = self#used_int
  method used_int = ()
  method unused_fun () = self#used_fun ()
  method used_fun () = ()
end

let () =
  ignore immediate#used_int;
  ignore immediate#internally_used_int;
  immediate#used_fun ();
  immediate#internally_used_fun ()

let () =
  ignore (factory ())#used_int;
  ignore (factory ())#internally_used_int;
  (factory ())#used_fun ();
  (factory ())#internally_used_fun ()

let () =
  ignore unexported_immediate#used_int;
  unexported_immediate#used_fun ()

let () =
  ignore (unexported_factory ())#used_int;
  (unexported_factory ())#used_fun ()
