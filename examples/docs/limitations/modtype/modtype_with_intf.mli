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

module Regular : T

module With : T with type t = int

module Subst : T with type t := int

module Incl : sig
  include T
end

module Incl_with : sig
  include T with type t = int
end

module Incl_subst : sig
  include T with type t := int
end

module Ftor () : T

module Ftor_with () : T with type t = int

module Ftor_subst () : T with type t := int
