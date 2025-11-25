(* The results are organized by section in the dead_code_analyzer's output. *)
type t =
  | Constr_and_fields
  | Methods
  | Opt_always
  | Opt_never
  | Style
  | Threshold of int * t
  | Values

val to_string : t -> string

(* Compares according to the order of sections in the reports.
  E.g.
   `compare Values Opt_never` is `< 0` because the "UNUSED EXPORTED VALUES"
   section appears before the "OPTIONAL ARGUMENTS: NEVER"*)
val compare : t -> t -> int

(* The test suite files have extensions corresponding to the sections of
   the expected results they contain. *)
val to_extension : t -> string

(* The test suite files have extensions corresponding to the sections of
   the expected results they contain. *)
val of_extension : string -> t option

(* The sections are preceded by headers to help identify them.
   Returns the corresponding section if the given string is header.
   Returns None otherwise. *)
val of_header : string -> t option

(* Folowing the header's title is a separator to indicate the start of the
   report for that section *)
val is_start : string -> bool
(* Identify subsections start. A sub start is a start*)
val is_sub_start : string -> bool

(* There is a footer at the end of each section to indicate the reporting for
   that section is done *)
val is_end : string -> bool
(* Identify subsections ending. A sub end is an end*)
val is_sub_end : string -> bool
