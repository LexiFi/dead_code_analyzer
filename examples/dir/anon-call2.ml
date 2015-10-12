let f x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn2.f ~a:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn2.f ~a:12) l) in ignore (tmp [])

let g x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn2.g ~a:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn2.g ~b:12 ~a:12) l) in ignore (tmp [])

let h x =
  if x > 32 then let tmp = (fun l -> List.map (AnonFn2.g ~b:12) l) in ignore (tmp [])
  else if x mod 4 <> 0 then let tmp = (fun l -> List.map (AnonFn2.f ~b:12) l) in ignore (tmp [])
