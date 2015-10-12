module type T = sig
  val nothing: unit -> unit
end

module I: T

module F(I: T): sig
  module M: T
end
