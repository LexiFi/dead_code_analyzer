(* modtype_of_with_intf.mli *)
module M : sig
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

module Regular : module type of M

module With : module type of M with type t = int

module Subst : module type of M with type t := int

module Incl : sig
  include module type of M
end

module Incl_with : sig
  include module type of M with type t = int
end

module Incl_subst : sig
  include module type of M with type t := int
end

module Ftor () : module type of M

module Ftor_with () : module type of M with type t = int

module Ftor_subst () : module type of M with type t := int
