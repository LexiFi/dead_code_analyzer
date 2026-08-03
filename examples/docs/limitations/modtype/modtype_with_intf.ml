(* modtype_with_intf.mli *)
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
