(* modtype_of_without_intf.ml *)
module M = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  (* to use as [always] in call to [f] *)
  let always = 0
end

module Regular : module type of M = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module With : module type of M with type t = int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Subst : module type of M with type t := int = struct
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
  include module type of M
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
  include module type of M with type t = int
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
  include module type of M with type t := int
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

module Ftor() : module type of M = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_with() : module type of M with type t = int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_subst() : module type of M with type t := int = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end
