module type S = sig
  val unused : int
  val used : int
  val internally_used :int
  val externally_used :int
end

include S
