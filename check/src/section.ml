type t =
  | Constr_and_fields
  | Methods
  | Opt_always
  | Opt_never
  | Style
  | Threshold of int * t
  | Values

let rec to_string = function
  | Constr_and_fields -> "Constr_and_fields"
  | Methods -> "Methods"
  | Opt_always -> "Opt_always"
  | Opt_never -> "Opt_never"
  | Style -> "Style"
  | Values -> "Values"
  | Threshold (n, t) ->
    let sub_string = to_string t in
    Printf.sprintf "Threshold(%d, %s)" n sub_string

let compare = compare

let rec to_extension = function
  | Constr_and_fields -> ".mlit"
  | Methods -> ".mlio"
  | Opt_always -> ".mlopta"
  | Opt_never -> ".mloptn"
  | Style -> ".mlstyle"
  | Values -> ".mli"
  | Threshold (n, base) -> to_extension base ^ string_of_int n

let rec of_extension = function
  | ".mlit" -> Some Constr_and_fields
  | ".mlio" -> Some Methods
  | ".mlopta" -> Some Opt_always
  | ".mloptn" -> Some Opt_never
  | ".mlstyle" -> Some Style
  | ".mli" -> Some Values
  | ext ->
    let try_threshold prefix =
      if String.starts_with ~prefix ext then
        let fmt = Scanf.format_from_string (prefix ^ "%d") "%d" in
        try
          let n = Scanf.sscanf ext fmt Fun.id in
          of_extension prefix
          |> Option.map (fun constr -> Threshold (n, constr))
        with Scanf.Scan_failure _ -> None
      else None
    in
    let exts = [".mlit"; ".mlio"; ".mlopta"; ".mloptn"; ".mlstyle"; ".mli"] in
    List.find_map try_threshold exts


let is_start s =
  String.for_all (( = ) '=') s (* = is used for main sections *)
  || String.for_all (( = ) '~') s (* ~ is used for subsections *)

let is_end s =
  s = "Nothing else to report in this section" (* main sections ending *)
  || String.for_all (( = ) '-') s (* subsections ending *)

let of_header = function
  | ".> UNUSED CONSTRUCTORS/RECORD FIELDS:" -> Some Constr_and_fields
  | ".> UNUSED METHODS:" -> Some Methods
  | ".> OPTIONAL ARGUMENTS: ALWAYS:" -> Some Opt_always
  | ".> OPTIONAL ARGUMENTS: NEVER:" -> Some Opt_never
  | ".> CODING STYLE:" -> Some Style
  | ".> UNUSED EXPORTED VALUES:" -> Some Values
  | header ->
    let get_threshold prefix constr =
      if String.starts_with ~prefix header then
        let fmt = Scanf.format_from_string (prefix ^ " %d time(s)") "%d" in
        let n = Scanf.sscanf header fmt Fun.id in
        Some (Threshold (n, constr))
      else None
    in
    let get_threshold_constr_and_fields () =
      let prefix = ".>->  ALMOST UNUSED CONSTRUCTORS/RECORD FIELDS: Called" in
      get_threshold prefix Constr_and_fields
    in
    let get_threshold_methods () =
      let prefix = ".>->  ALMOST UNUSED METHODS: Called" in
      get_threshold prefix Methods
    in
    let get_threshold_opt_always () =
      let prefix = ".>->  OPTIONAL ARGUMENTS: ALMOST ALWAYS: Except" in
      get_threshold prefix Opt_always
    in
    let get_threshold_opt_never () =
      let prefix = ".>->  OPTIONAL ARGUMENTS: ALMOST NEVER: Except" in
      get_threshold prefix Opt_never
    in
    let get_threshold_values () =
      let prefix = ".>->  ALMOST UNUSED EXPORTED VALUES: Called" in
      get_threshold prefix Values
    in
    let getters = [
      get_threshold_constr_and_fields;
      get_threshold_methods;
      get_threshold_opt_always;
      get_threshold_opt_never;
      get_threshold_values
    ]
    in
    List.find_map (fun f -> f ()) getters
