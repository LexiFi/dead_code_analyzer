module M = struct
  let immediate_externally_used_by_param_requirement = object method m = 42 end
  let immediate_externally_used_by_val_requirement = object method m = 42 end
  let immediate_externally_used_by_val_constraint = object method m = 42 end
  let immediate_internally_used_by_param_requirement = object method m = 42 end
  let immediate_internally_used_by_val_requirement = object method m = 42 end
  let immediate_internally_used_by_val_constraint = object method m = 42 end
  let immediate_unused = object method m = 42 end

  class class_externally_used_by_param_requirement = object method m = 42 end
  class class_externally_used_by_val_requirement = object method m = 42 end
  class class_externally_used_by_val_constraint = object method m = 42 end
  class class_internally_used_by_param_requirement = object method m = 42 end
  class class_internally_used_by_val_requirement = object method m = 42 end
  class class_internally_used_by_val_constraint = object method m = 42 end
  class class_unused = object method m = 42 end
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

let expect_param (module Param : PARAM) = ()

module type INTERNAL_PARAM = sig
  val immediate_internally_used_by_param_requirement : < m : int >
  class class_internally_used_by_param_requirement : object method m : int end
end

module type INTERNAL_VAL_REQ = sig
  val immediate_internally_used_by_val_requirement : < m : int >
  class class_internally_used_by_val_requirement : object method m : int end
end

module type INTERNAL_VAL_CSTR = sig
  val immediate_internally_used_by_val_constraint : < m : int >
  class class_internally_used_by_val_constraint : object method m : int end
end

let () =
  let _ = (module M : INTERNAL_VAL_CSTR) in
  let _ : (module INTERNAL_VAL_REQ) = (module M) in
  let expect_param (module Param: INTERNAL_PARAM) = () in
  expect_param (module M)
