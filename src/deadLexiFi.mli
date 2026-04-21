val sig_value : (Types.value_description -> unit) ref

val export_type : (Lexing.position -> string -> unit) ref

val type_ext : (Typedtree.core_type -> unit) ref

val type_decl : (Typedtree.type_declaration -> unit) ref

val tstr_type : (Typedtree.type_declaration -> string -> unit) ref

val ttype_of : (Typedtree.expression -> unit) ref

val prepare_report : ((Lexing.position, string * string) Hashtbl.t -> unit) ref

val set_hooks : unit -> unit
(** Sets the extensions used internally at LexiFi. *)
