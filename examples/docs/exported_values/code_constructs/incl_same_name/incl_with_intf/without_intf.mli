(* incl_with_intf/without_intf.mli *)
include module type of To_incl.Without_intf
(* WARNING: Line placement is important. It must match the one found in the original file *)
val redefined_unused : int
val redefined_used : int

