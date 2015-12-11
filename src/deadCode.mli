(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

(** Dead code anlyzing tool. It only reports unused exported values, constructors/record fields
  and methods by default.
  Options can enable reporting of optional arguments always/never used as bad style of code.
  In addition to selecting which reports are to be displayed, the limit of authorized
  occurences needed to be reported can be selected (default is 0).

  It assumes .mli/.mfi are compiled with {e -keep-locs} and .ml/.mf are compiled with {e -bin-annot}.


  {2 Unused Exported Values}

  This section reports the following when exported:

  - Values never accessed.
      e.g. {[
        let x = 0
        let () = ()
      ]}
      Here, [x] will be reported.

  As this section focuses on exported values, internal calls are ignored by default.
  This will lead the analyzer to focus on values that are uselessly exported.
  To keep track of all uses, call the {e internal} option.

  The {e thresholdE} option can be used to report values the rule respecting above most of the time.
  e.g. calling the dead code analyzer with {e --thresholdE 1} on
     {[
        let x = 0
        let () = ignore x
      ]}
  will lead to reporting [x] in the almost unused subsection because its number of use <= 1.

  The {e call-sites} option can be used to print the call sites of values reported as almost unused.


  {2 Unused Constructors/Record Fields}

  - Record fields not read.
      e.g. {[
        type t = {foo: int; bar: bool}
        let r = {foo = 0; bar = false}
        let x = {r with bar = true}
        let () = ignore x.foo
      ]}
      Here, [t.bar] will be reported.

  - Constructors never constructed.
      e.g. {[
        type t = Foo | Bar
        let x = Foo
        let () = match x with
          | Bar -> ()
          | _ -> ()
      ]}
      Here, [t.Bar] will be reported.

  Types are studied as a whole and iff they are explicitly declared as their own types.
  Consequently, signatures as [val f: [`A | `B] -> unit] cannot lead to reporting [`A] or [`B].

  The {e thresholdT} option work similarily to the {e thresholdE}
  and the {e call-sites} option can also be used


  {2 Unused Methods}

  - Public methods never called.
      e.g. {[
        class p = object
          method f = ()
        end
        class c = object
          inherit p
          method g = ()
        end
        let () = c#f
      ]}
      Here, [c#g] will be reported.

  It has to be noted that calling [c#f] is in fact the same as calling [p#f].
  The owner is considered to be the one who defines the function.

  The {e thresholdC} option work similarily to the {e thresholdE}
  and the {e call-sites} option can also be used


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
  {e ./deadCode.<ext> <options> <directory|file>}

  For more informations use the {e --help} option

  *)
