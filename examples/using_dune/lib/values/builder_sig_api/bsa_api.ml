module type PARAM = sig
  val used_by_functor_app : int
  val used : int
end

module type S = sig
  val unused : int
  val used : int
  val internally_used :int
  val externally_used :int
  val sometimes_used : int
end
