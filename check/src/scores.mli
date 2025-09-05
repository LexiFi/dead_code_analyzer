type t

val to_string : t -> string

val pp : t -> unit

val total : t -> int
val expected : t -> int
val failed : t -> int

val extract_total : string -> int option
val extract_success : string -> int option
val extract_failed : string -> int option

val init : t

val incr_fp : t -> t
val incr_fn : t -> t
val incr_success : t -> t

val set_success : int -> t -> t
val set_failures : int -> t -> t
