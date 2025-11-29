class unused : object method unused : unit end

class empty : object  end

class basics :
  object
    val unused_int_instance_var : int
    val used_int_instance_var : int
    method never_used_int : int
    method sometimes_used_int : int
    method always_used_int : int
    method internally_used_int : int
    method externally_used_int : int
    method never_used_fun : unit -> unit
    method sometimes_used_fun : unit -> unit
    method always_used_fun : unit -> unit
    method internally_used_fun : unit -> unit
    method externally_used_fun : unit -> unit
  end

val basics_factory : unit -> basics

class unused_fun_class : unit -> object method unused : unit end

class fun_class :
  unit -> object
    method unused : unit -> unit
    method used : unit -> unit
  end

class fun_class_constructor : unit -> fun_class

val fun_class_factory : unit -> fun_class

class _self_used :
  object
    method unused : unit -> unit
    method used : unit -> unit
  end

class _self_used_fun_class :
  unit -> object
    method unused : unit -> unit
    method used : unit -> unit
  end
