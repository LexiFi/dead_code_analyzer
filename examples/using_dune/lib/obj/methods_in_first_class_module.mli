module M : sig
  val immediate_externally_used_by_param_requirement : < m : int >
  val immediate_externally_used_by_val_requirement : < m : int >
  val immediate_externally_used_by_val_constraint : < m : int >
  val immediate_internally_used_by_param_requirement : < m : int >
  val immediate_internally_used_by_val_requirement : < m : int >
  val immediate_internally_used_by_val_constraint : < m : int >
  val immediate_unused : < m : int >

  class class_externally_used_by_param_requirement : object method m : int end
  class class_externally_used_by_val_requirement : object method m : int end
  class class_externally_used_by_val_constraint : object method m : int end
  class class_internally_used_by_param_requirement : object method m : int end
  class class_internally_used_by_val_requirement : object method m : int end
  class class_internally_used_by_val_constraint : object method m : int end
  class class_unused : object method m : int end
end

module type PARAM = sig
  val immediate_externally_used_by_param_requirement : < m : int >
  class class_externally_used_by_param_requirement : object method m : int end
end

module type VAL_REQ = sig
  val immediate_externally_used_by_val_requirement : < m : int >
  class class_externally_used_by_val_requirement : object method m : int end
end

module type VAL_CSTR = sig
  val immediate_externally_used_by_val_constraint : < m : int >
  class class_externally_used_by_val_constraint : object method m : int end
end

val expect_param : (module PARAM) -> unit

