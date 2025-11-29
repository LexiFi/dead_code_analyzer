class unused = object
  method unused = ()
end

class empty = object end

class basics = object
  val unused_int_instance_var = 42
  val used_int_instance_var = 42
  method never_used_int = used_int_instance_var
  method sometimes_used_int = 42
  method always_used_int = 42
  method internally_used_int = 42
  method externally_used_int = 42
  method never_used_fun () = ()
  method sometimes_used_fun () = ()
  method always_used_fun () = ()
  method internally_used_fun () = ()
  method externally_used_fun () = ()
end

let basics_factory () : basics = new basics

class unused_fun_class () = object
  method unused = ()
end

class fun_class () = object
  method unused () = ()
  method used () = ()
end

class fun_class_constructor () = fun_class ()

let fun_class_factory = new fun_class

class _self_used = object (self)
  method unused () = self#used ()
  method used () = ()
end

class _self_used_fun_class () = object (self)
  method unused () = self#used ()
  method used () = ()
end

let () =
  let e = new empty in
  ignore e

let () =
  let b_use_most = new basics in
  ignore b_use_most#always_used_int;
  ignore b_use_most#sometimes_used_int;
  ignore b_use_most#internally_used_int;
  b_use_most#always_used_fun ();
  b_use_most#sometimes_used_fun ();
  b_use_most#internally_used_fun ()

let () =
  let b_use_least = new basics in
  ignore b_use_least#always_used_int;
  ignore b_use_least#internally_used_int;
  b_use_least#always_used_fun ();
  b_use_least#internally_used_fun ()

let () =
  let bf_use_most = basics_factory () in
  ignore bf_use_most#always_used_int;
  ignore bf_use_most#sometimes_used_int;
  ignore bf_use_most#internally_used_int;
  bf_use_most#always_used_fun ();
  bf_use_most#sometimes_used_fun ();
  bf_use_most#internally_used_fun ()

let () =
  let bf_use_least = basics_factory () in
  ignore bf_use_least#always_used_int;
  ignore bf_use_least#internally_used_int;
  bf_use_least#always_used_fun ();
  bf_use_least#internally_used_fun ()

let () =
  let fc = new fun_class () in
  fc#used ();
  let fcc = new fun_class_constructor () in
  fcc#used ();
  let fcf = fun_class_factory () in
  fcf # used ()
