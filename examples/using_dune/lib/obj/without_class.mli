val immediate :
  < unused_int : int;
    used_int : int;
    internally_used_int : int;
    externally_used_int : int;
    unused_fun : unit -> unit;
    used_fun : unit -> unit;
    internally_used_fun : unit -> unit;
    externally_used_fun : unit -> unit >

val factory :
  unit ->
  < unused_int : int;
    used_int : int;
    internally_used_int : int;
    externally_used_int : int;
    unused_fun : unit -> unit;
    used_fun : unit -> unit;
    internally_used_fun : unit -> unit;
    externally_used_fun : unit -> unit >

val _self_used_immediate :
  < unused_int : unit;
    used_int : unit;
    unused_fun : unit -> unit;
    used_fun : unit -> unit >

val _self_used_factory :
  unit ->
  < unused_int : unit;
    used_int : unit;
    unused_fun : unit -> unit;
    used_fun : unit -> unit >
