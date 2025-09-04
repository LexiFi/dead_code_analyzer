type t

val to_string : t -> string

val pp : t -> unit

val init : t

val incr_fp : t -> t
val incr_fn : t -> t
val incr_success : t -> t
