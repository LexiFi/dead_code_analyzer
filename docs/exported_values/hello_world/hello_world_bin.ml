(* hello_world_bin.ml *)
let () =
  let open Hello_world_lib in
  let hello_world = hello ^ world in
  let goodbye_world = goodbye ^ world in
  print_endline hello_world
