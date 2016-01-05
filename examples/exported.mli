module type T = sig
  val five : int
end

module M1 : sig
  val five : int
end

module F (M: T) : sig
  val plus_five : int -> int
  val times_five : int -> int
end

module M2 : sig
  val five : int
  val plus_five : int -> int
  val ten : int
end
