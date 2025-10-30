class type ct = object
  method f : unit -> unit
  method g : unit -> unit
  method h : unit -> unit
end

class p : ct

class c : object
  inherit p
  method h : unit -> unit
end
