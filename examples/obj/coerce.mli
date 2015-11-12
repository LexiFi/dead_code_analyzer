class type ct = object
  method h : unit -> unit
end

class p : ct

class c : object
  inherit p
  method h : unit -> unit
end
