open With_class

let () =
  let b_use_most = new basics in
  ignore b_use_most#always_used_int;
  ignore b_use_most#sometimes_used_int;
  ignore b_use_most#externally_used_int;
  b_use_most#always_used_fun ();
  b_use_most#sometimes_used_fun ();
  b_use_most#externally_used_fun ()

let () =
  let b_use_least = new basics in
  ignore b_use_least#always_used_int;
  ignore b_use_least#externally_used_int;
  b_use_least#always_used_fun ();
  b_use_least#externally_used_fun ()

let () =
  let bf_use_most = basics_factory () in
  ignore bf_use_most#always_used_int;
  ignore bf_use_most#sometimes_used_int;
  ignore bf_use_most#externally_used_int;
  bf_use_most#always_used_fun ();
  bf_use_most#sometimes_used_fun ();
  bf_use_most#externally_used_fun ()

let () =
  let bf_use_least = basics_factory () in
  ignore bf_use_least#always_used_int;
  ignore bf_use_least#externally_used_int;
  bf_use_least#always_used_fun ();
  bf_use_least#externally_used_fun ()

let () =
  let fc = new fun_class () in
  fc#used ();
  let fcc = new fun_class_constructor () in
  fcc#used ();
  let fcf = fun_class_factory () in
  fcf # used ()

let is_used = ref false
let mark_used () =
  is_used := true
