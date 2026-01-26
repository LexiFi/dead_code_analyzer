include Bsa_builder.Make(struct
    let used_by_functor_app = 42
    let used = 42
  end)

let () =
  ignore used;
  ignore internally_used
