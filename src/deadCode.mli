(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values by default.
  Options can enable reporting of optional arguments always/never used and bad style of code.
  In addition to selecting which reports are to be displayed, the threshold of authorized
  occurences needed to be reported can be selected (default is 0).

  It assumes .mli are compiled with {e -keep-locs} and .ml are compiled with {e -bin-annot}.


  {2 Unused Exported Values}

  This section reports the following when exported:

  - Record fields not read.
      e.g. {[
        type t = {foo: int; bar: bool}
        let r = {foo = 0; bar = false}
        let x = {r with bar = true}
        let () = ignore x.foo
      ]}
      Here, [t.bar] will be reported.

  - Variant constructors never constructed.
      e.g. {[
        type 'a option = Foo | Bar
        let x = Foo
        let () = match x with
          | Bar -> ()
          | _ -> ()
      ]}
      Here, [t.Bar] will be reported as it is never built.

  - Values never accessed.
      e.g. {[
        let x = 0
        let () = ()
      ]}
      Here, [x] will be reported as it is declared but never used

  Types are studied as a whole and iff they are explicitly declared as their own types.
  Consequently, signatures as [val f: [`A | `B] -> unit] cannot lead to reporting [`A] or [`B].

  As this section focuses on exported values, internal calls are ignored by default.
  This will lead the analyzer to focus on values that are uselessly exported.
  To keep track of all uses, call the {e internal} option.

  The {e threshold} option can be used to report values and types respecting the rules above
  most of the time. e.g. calling the dead code analyzer with {e --threshold +1} on
     {[
        let x = 0
        let () = ignore x
      ]}
  will lead to reporting [x] in the almost unused subsection because its number of use <= 1.

  The {e call-sites} option can be used to print the call sites of values reported as almost unused.


  {2 Optional Arguments}

  Optional arguments always used (with a value <> [None])
  and never used (or only with [None]) are reported.

  This reporting is done regardless of whether the function an optional argument is
  attached to is exported.

      e.g. {[
        let f ?a ?b () = ()
        let () = f ~a:0 ()
      ]}
  Here [?a] is reported as always used and [?b] never.

  The {e threshold} option can be used to report optional arguments that are use/unused
  most of the time. e.g. calling the dead code analyzer with {e --threshold +0.6} on
      {[
        let f ?a ?b () = ()
        let () =
          f ~a:0 ()
          |> f ~a:0 ~b:0
          |> f ~a:0 ~b:0
      ]}
  will lead to reporting both [?a] in the always used section
and [?b] in the almost always used subsection.

  Another usage of the option can in addition to the percentage of use verify the number
  of occurences as for the exported values. e.g. calling the dead code analyzer with
  {e --threshold +1+0.6+both} on
      {[
        let f ?a ?b () = ()
        let () =
          f ~a:0 ()
          |> f ~a:0 ~b:0
          |> f ~a:0 ~b:0
      ]}
  will lead to reporting [?a] but not [?b] because number of time it is used is > 1
  and the percentage of time it is unused is < 60%.

  The {e call-sites} option can be used to print the call sites of values reported as
  almost always/never used.


  {2 Coding Style}

  This section focuses on:
    - use of unit patterns as in [let f : unit -> unit = fun x -> ()].
      In this example x is reported as a unit pattern.
      Any value of type [unit] different from [()]
      like [let _ = ()] or [x] in the previous example is reported as a unit pattern.

    - unit bindings where sequencing should be used: [let () = ... in ...]

    - useless bindings of the form [let x = ... in x]

    - arguments expecting optional argument i.e. [val f: ... -> (... -> ?_:_ -> ...) -> ...]


  {2 Usage}
  {e ./dead_code.byt <options> <directory|file>}

  For more informations use the {e --help} option

  *)


val bad_files : string list ref
  (** Files which could not be read correctly *)

val type_dependencies : (Location.t * Location.t) list ref
  (** Like the cmt value_dependencies but for types. *)

(** Value informations. *)
type vd_node = {
  loc : Location.t;
    (** Declaration. *)
  mutable opt_args : string list;
    (** Expected optional arguments. *)
  mutable ptr : vd_node;
    (** Value it binds to (or call last). Points to itself by default. *)
}

(** Optional argument informations. *)
type opt_arg = {
  mutable with_val : Location.t list;
    (** Sites where called with a value. *)
  mutable without_val : Location.t list;
    (** Sites where called without value (or None). *)
}


(** Flags and their processing. *)
module DeadFlag : sig

  type threshold = {
    exceptions : int;
      (** Maximum number of exceptions to still be part of a section. *)
    percentage : float;
      (** Minimum percentage of valid cases to be part of a section (for optional arguments). *)
    optional : [ `Both | `Percent ];
      (** Constraints optional arguments must respect to be reported part of a section. *)
  }

  type opt = {
    always : bool;
      (** Display the "OPTIONAL ARGUMENTS: ALWAYS" section. *)
    never : bool;
      (** Display the "OPTIONAL ARGUMENTS: NEVER" section. *)
    call_sites : bool;
      (** Display the call sites for optional arguments. *)
  }

  type style = {
    opt_arg : bool;
      (** Expect argument with optional argument. *)
    unit_pat : bool;
      (** Unit pattern. *)
    seq : bool;
      (** Sequence should be used. *)
    binding : bool;
      (** Useless binding. *)
  }

  type exported = {
    print : bool;
      (** Display the "UNUSED EXPORTED VALUES" section. *)
    call_sites : bool;
      (** Display the call sites. *)
  }

  val verbose : bool ref
    (** Verbose mode: print currently processed filename. *)

  val underscore : bool ref
    (** Ignore names starting with an underscore. *)

  val types : string list ref
    (** Ignores values of certain types. *)

  val internal : bool ref
    (** Keep internal calls. *)
end

(** type_expr manipulation *)
module DeadType : sig
  val to_string : Types.type_expr -> string
    (** [to_string typ] converts [typ] to its string representation in the toplevel *)

  val match_str : Types.type_expr -> string -> bool
    (** [match_str typ str] checks if a [typ] matches [str].
      [str] must be formated as the toplevel representation representation of the expected type. *)

  val check_style : Types.type_expr -> Location.t -> unit
    (** Look for bad style typing. (i.e. Argument expecting an optional argument) *)
end


(** {2 Pretty Printing} *)

val section : ?sub:bool -> string -> unit
  (** Formats and prints (sub)section title. *)

val separator : unit -> unit
  (** Prints the end of section delimiter. *)

val report_unused_exported : unit -> unit
  (** Prints the "EXPORTED UNUSED VALUES" section and subsections contents. *)

val report_opt_args : string -> (Location.t * string * opt_arg) list -> unit
  (** [report_opt_args used args] prints the "OPTIONAL ARGUMENTS" section and subsections contents.
    [used] must be "NEVER" to report optional arguments never used;
    anything else reports the ones always used. It is printed as part of the section title.
    [args] list of optional arguments that have been processed *)

val report_style : unit -> unit
  (** Print the "CODING STYLE" section contents. *)

val prloc : ?fn:string -> Location.t -> unit
  (** Print a location as: `path/filename:line: '. *)


(** {2 Node processing} *)

val vd_node : ?add:bool -> Location.t -> vd_node
  (** [vd_node ?add loc] gives the node corresponding to [loc] if it exists, build it otherwise
    and, if add, save it for later use.*)

val merge_locs : ?add:bool -> Location.t -> Location.t -> unit
  (** [merge_locs ?add loc1 loc2] as [(vd_node ?add n1).ptr] pointing to [n2.ptr] *)


val analyze_opt_args : unit -> (Location.t * string * opt_arg) list
  (** Process information about optional arguments for later reporting. *)


val collect_references : Tast_mapper.mapper
  (** Run through the ast. *)

val collect_export : Ident.t list -> string -> Types.signature_item -> unit
  (** [collect_export path name signature] process the [signature] to find the value to export.
    [path] is the current module-path. [name] is the current filename's basename without extension *)
