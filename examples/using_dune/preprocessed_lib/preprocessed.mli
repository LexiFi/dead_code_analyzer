val unused : int
val used : int
val internally_used : int
val externally_used : int

val immediate : <
  unused : int;
  used : int;
  internally_used : int;
  externally_used : int;
>

type constructors =
    Unused
  | Used
  | Internally_used
  | Externally_used

type constr_with_eq = Unused
[@@deriving eq]

type record = {
  unused : int;
  used : int;
  internally_used : int;
  externally_used : int;
}

type record_with_eq = {implicitly_used : int} [@@deriving eq]

val exported_f :
  ?never:'a ->
  ?always:'b ->
  ?internally:'c ->
  ?externally:'d ->
  unit -> unit

val internally_used_f :
  ?never:'a ->
  ?always:'b ->
  ?sometimes:'c ->
  unit -> unit

val externally_used_f :
  ?never:'a ->
  ?always:'b ->
  ?sometimes:'c ->
  unit -> unit
