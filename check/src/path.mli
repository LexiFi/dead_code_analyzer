(* Convert windows and unix style separator (resp. '\\' and '/') to
   the system's separator, remove any intermediate reference to
   the current directory ("."), and reduce multiple consecutive separators
   into 1.
   WARNING: If `path` is a relative path, it will add "./" at the beginning
   after the above manipulation *)
val normalize : string -> string

(* Same as normalize but the result's separator is Unix's ('/') *)
val normalize_to_unix : string -> string

(* Paths read in `.got` files point to files in '<project_root>/examples/'
   using absolute paths :
     - for files in the 'using_make' subdirectory :
       '<project_root>/examples/using_make/<path/to/file>' with
       '<project_root>' an absolute path.
     - for files in the 'using_dune' subdirectory :
       '<project_root>/examples/using_dune/_build/default/<path/to/file>'
       with '<project_root>' an absolute path.
   We want to relocate them as relative to the <project_root> to compare them
   with paths in `.exp` files independently of the project's location :
   './examples/using<dune|make>/<path/to/file>'. *)
val relocate : string -> string
