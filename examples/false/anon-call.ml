let f x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn.f ~a:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn.f) l) in ignore (tmp [])

let g x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn.g ~a:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn.g ~a:12) l) in ignore (tmp [])

let h x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn.h ~b:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn.h ~a:12) l) in ignore (tmp [])
