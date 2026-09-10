let isatty fd =
  #scope
  #if OCAML_VERSION >= (4, 14, 0) && OCAML_VERSION < (5, 1, 0)
  #define Out_channel Unix
  #endif
  match fd with
  | `Stdout -> Out_channel.isatty Out_channel.stdout
  | `Stderr -> Out_channel.isatty Out_channel.stderr
  #endscope


#if OCAML_VERSION >= (5, 1, 0)
let input_lines = In_channel.input_lines
#else
let [@tail_mod_cons] rec input_lines ic =
  (* reproduce https://github.com/ocaml/ocaml/blob/5.3.0/stdlib/in_channel.ml#L195 *)
  match In_channel.input_line ic with
  | Some line -> line :: input_lines ic
  | None -> []
#endif
