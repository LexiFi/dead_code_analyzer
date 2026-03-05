(* include_bin.ml *)
let () =
  let open Include_lib in
  ignore Original.used_directly;
  ignore Reexport.used_indirectly;
  ignore Redefine.used_directly;
