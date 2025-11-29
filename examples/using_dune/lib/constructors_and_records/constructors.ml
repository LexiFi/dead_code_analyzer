type unused_single_no_param = Unused_single_no_param
type unused_single_unit = Unused_single_unit of unit
type 'a unused_single_param = Unused_single_param of 'a
type 'a unused_mix =
  | Unused_no_param
  | Unused_unit of unit
  | Unused_param of 'a

type matched_single_no_param = Matched_single_no_param
type matched_single_unit = Matched_single_unit of unit
type 'a matched_single_param = Matched_single_param of 'a
type 'a matched_mix =
  | Matched_no_param
  | Matched_unit of unit
  | Matched_param of 'a

type used_single_no_param = Used_single_no_param
type used_single_unit = Used_single_unit of unit
type 'a used_single_param = Used_single_param of 'a
type 'a used_mix =
  | Used_no_param
  | Used_unit of unit
  | Used_param of 'a

type internally_used_single_no_param = Internally_used_single_no_param
type internally_used_single_unit = Internally_used_single_unit of unit
type 'a internally_used_single_param = Internally_used_single_param of 'a
type 'a internally_used_mix =
  | Internally_used_no_param
  | Internally_used_unit of unit
  | Internally_used_param of 'a

type externally_used_single_no_param = Externally_used_single_no_param
type externally_used_single_unit = Externally_used_single_unit of unit
type 'a externally_used_single_param = Externally_used_single_param of 'a
type 'a externally_used_mix =
  | Externally_used_no_param
  | Externally_used_unit of unit
  | Externally_used_param of 'a

type 'a partially_used_mix_1 =
  | Partially_used_no_param (* unused *)
  | Partially_used_unit of unit
  | Partially_used_param of 'a

type 'a partially_used_mix_2 =
  | Partially_used_no_param
  | Partially_used_unit of unit (* unused *)
  | Partially_used_param of 'a

type 'a partially_used_mix_3 =
  | Partially_used_no_param
  | Partially_used_unit of unit
  | Partially_used_param of 'a (* unused *)

let () = (* match only *)
  let _ = function
    | Matched_single_no_param -> ()
  in
  let _ = function
    | Matched_single_unit () -> ()
  in
  let _ = function
    | Matched_single_param _ -> ()
  in
  let _ = function
    | Matched_no_param
    | Matched_unit ()
    | Matched_param _ -> ()
  in
  ()

let () = (* match only *)
  let _ = function
    | Used_single_no_param -> ()
  in
  let _ = function
    | Used_single_unit () -> ()
  in
  let _ = function
    | Used_single_param _ -> ()
  in
  let _ = function
    | Used_no_param
    | Used_unit ()
    | Used_param _ -> ()
  in
  ()

let () = (* construct only *)
  let _ = Used_single_no_param in
  let _ = Used_single_unit () in
  let _ = Used_single_param 42 in
  let _ = Used_no_param in
  let _ = Used_unit () in
  let _ = Used_param 42 in
  ()

let () = (* match only *)
  let _ = function
    | Internally_used_single_no_param -> ()
  in
  let _ = function
    | Internally_used_single_unit () -> ()
  in
  let _ = function
    | Internally_used_single_param _ -> ()
  in
  let _ = function
    | Internally_used_no_param
    | Internally_used_unit ()
    | Internally_used_param _ -> ()
  in
  ()

let () = (* construct only *)
  let _ = Internally_used_single_no_param in
  let _ = Internally_used_single_unit () in
  let _ = Internally_used_single_param 42 in
  let _ = Internally_used_no_param in
  let _ = Internally_used_unit () in
  let _ = Internally_used_param 42 in
  ()

let () = (* construct only *)
  let _ : _ partially_used_mix_1 = Partially_used_unit () in
  let _ : _ partially_used_mix_2 = Partially_used_param 42 in
  let _ : _ partially_used_mix_3 = Partially_used_no_param in
  ()
