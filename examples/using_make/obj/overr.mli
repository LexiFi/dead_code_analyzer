class p1 : object
  method f : unit -> unit
  method g : unit -> unit
  method h : unit -> unit
end

class p2 : object
  method g : unit -> unit
  method h : unit -> unit
end

class c : object
  method f : unit -> unit
  inherit p1
  inherit p2
  method g : unit -> unit
end
