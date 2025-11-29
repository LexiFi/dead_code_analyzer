val used_int : int
val unused_int : int
val internally_used_int : int
val externally_used_int : int
val externally_used_as_parameter_int : int

val externally_written_bool_ref : bool ref
val externally_matched_option : bool option

val externally_stored_in_ref_bool : bool
val externally_stored_in_option_bool : bool

val used_fun : 'a -> 'a
val unused_fun : 'a -> 'a
val internally_used_fun : 'a -> 'a
val externally_used_fun : 'a -> 'a
val externally_used_as_parameter_fun : 'a -> unit

val used_alias_fun : 'a -> 'a
val unused_alias_fun : 'a -> 'a
val externally_used_alias_fun : 'a -> 'a

val partially_used_fun : 'a -> 'b -> 'a * 'b
val used_closure : 'a -> 'b -> 'a * 'b
val unused_closure : 'a -> 'b -> 'a * 'b
val internally_used_closure : 'a -> 'b -> 'a * 'b
val externally_used_closure : 'a -> 'b -> 'a * 'b

val _used_underscore_fun : 'a -> 'a
val _unused_underscore_fun : 'a -> 'a
val _externally_used_underscore_fun : 'a -> 'a
