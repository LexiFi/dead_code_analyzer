module type T = sig
  val nothing: unit -> unit
end

module I: T = struct
  let nothing () = ()
end

module F(I: T) = struct
  module M = I
end
