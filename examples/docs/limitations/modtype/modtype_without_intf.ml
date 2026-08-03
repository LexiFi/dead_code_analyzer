(* modtype_without_intf.ml *)
module type T = sig
  type t
  type ctor = Ctor
  type field = {field : unit}
  val x : t
  val o : < m : t >
  class c : object method m : t end
  val f : ?always:t -> ?never:t -> unit -> t
  (* to use as [always] in call to [f] *)
  val always : t
end

module Regular : T = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module With : T with type t = int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Subst : T with type t := int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl : sig
  include T
end = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl_with : sig
  include T with type t = int
end = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl_subst : sig
  include T with type t := int
end = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor() : T = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_with() : T with type t = int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_subst() : T with type t := int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end
