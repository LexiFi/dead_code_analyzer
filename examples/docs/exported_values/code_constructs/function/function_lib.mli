(* function_lib.mli *)

val memoize : f:('a -> 'b) -> 'a -> 'b

val heavy_computation : 'a -> 'a

val unused : 'a -> 'a

val do_nothing : 'a -> unit
