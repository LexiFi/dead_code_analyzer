let () =
  let open Wrapped_lib.Mixed_args in
  interlaced_mandatory_and_opt () () () ~always:() ();
  interlaced_mandatory_and_opt () () () ~always:() ~sometimes:() ();
  interlaced_labeled_and_opt ~lab1:() ~always:() ~lab2:() ~lab3:() ();
  interlaced_labeled_and_opt ~lab1:() ~always:() ~lab2:() ~lab3:() ~sometimes:() ();
  interlaced_mandatory_labeled_and_opt ~lab1:() () ~always:() ~lab2:() ();
  interlaced_mandatory_labeled_and_opt ~lab1:() () ~always:() ~lab2:() ~sometimes:() ();
  ()

let is_used = ref false
let mark_used () =
  is_used := true
