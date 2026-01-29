module Parameter = struct
  let used_by_functor_app = 42
  let used = 42
end

include Bsa_builder.Make(Parameter)

let () =
  ignore Parameter.used;
  ignore used;
  ignore internally_used;
  ignore sometimes_used
