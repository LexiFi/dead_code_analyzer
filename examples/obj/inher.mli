class p : object
  method f : unit -> unit
  method g : unit -> unit
  method h : unit -> unit
end

class c : object
  inherit p
end

val o : c

val f: <f: unit -> unit> -> unit
