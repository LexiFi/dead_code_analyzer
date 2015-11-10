class p1 : object
  method f : unit -> unit
  method g : unit -> unit
end

class p2 : object
  method f : unit -> unit
end

class c : object
  inherit p1
  inherit p2
  method f : unit -> unit
  method g : unit -> unit
end
