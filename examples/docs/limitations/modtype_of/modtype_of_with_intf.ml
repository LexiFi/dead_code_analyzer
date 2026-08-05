(* modtype_of_with_intf.ml *)
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

module Regular = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module With = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Subst = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl_with = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Incl_subst = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor() = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_with() = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end

module Ftor_subst() = struct
  type t = int
  type ctor = Ctor
  type field = {field : unit}
  let x = 0
  let o = object method m = 0 end
  class c = object method m = 0 end
  let f ?always:_ ?never:_ () = 0
  let always = 0
end
