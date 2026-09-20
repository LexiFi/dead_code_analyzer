type t = {
  declarations : string Utils.StringHash.t Utils.LocHash.t;
    (** location of decl -> builddir -> cf_path *)
  uses : Utils.LocSet.t Utils.LocHash.t
}

let create () =
  let open Utils in
  let declarations = LocHash.create 256 in
  let uses = LocHash.create 256 in
  {declarations; uses}

let add_exported_declaration ~builddir ~cf_loc ~cf_path ctors_fields =
  let open Utils in
  let tbl =
    match LocHash.find_opt ctors_fields.declarations cf_loc with
    | Some tbl -> tbl
    | None ->
        let tbl = StringHash.create 8 in
        LocHash.add ctors_fields.declarations cf_loc tbl;
        tbl
  in
  (* Collisions at the same cf_loc in the same builddir should not happen.
      TODO: confirm [x] and [y] have different locs in [let x as y = ...] *)
  StringHash.replace tbl builddir cf_path;
  ctors_fields

let remove_exported_declaration ~builddir ~cf_loc ctors_fields =
  let open Utils in
  match LocHash.find_opt ctors_fields.declarations cf_loc with
  | None -> ctors_fields
  | Some tbl ->
      StringHash.remove tbl builddir;
      ctors_fields

let is_exported_declaration ~cf_loc ctors_fields =
  Utils.LocHash.mem ctors_fields.declarations cf_loc

let get_cf_path ~builddir ~cf_loc ctors_fields =
  let open Utils in
  match LocHash.find_opt ctors_fields.declarations cf_loc with
  | None -> None
  | Some tbl -> StringHash.find_opt tbl builddir

let add_use ~cf_loc ~use_loc ctors_fields =
  let open Utils in
  LocHash.find_set ctors_fields.uses cf_loc
  |> LocSet.add use_loc
  |> LocHash.replace ctors_fields.uses cf_loc;
  ctors_fields

let remove_uses ~cf_loc ctors_fields =
  Utils.LocHash.remove ctors_fields.uses cf_loc;
  ctors_fields

let get_uses ~cf_loc ctors_fields =
  let open Utils in
  LocHash.find_set ctors_fields.uses cf_loc
  |> LocSet.to_seq
  |> List.of_seq

let add_alias ~orig_loc ~alias_loc ctors_fields =
  let open Utils in
  let alias_uses = LocHash.find_set ctors_fields.uses alias_loc in
  let orig_uses = LocHash.find_set ctors_fields.uses orig_loc in
  let orig_uses = LocSet.union alias_uses orig_uses in
  LocHash.replace ctors_fields.uses orig_loc orig_uses;
  ctors_fields

let get_unused ?(max_uses=0) ctors_fields =
  let open Utils in
  let res = Hashtbl.create (max_uses + 1) in
  let add_if_unused cf_loc builddir =
    let nb_uses = LocSet.cardinal (LocHash.find_set ctors_fields.uses cf_loc) in
    if nb_uses <= max_uses then
      let locs =
        Hashtbl.find_opt res nb_uses
        |> Option.value ~default:[]
      in
      let locs = (cf_loc, builddir)::locs in
      Hashtbl.replace res nb_uses locs
  in
  let add_if_unused cf_loc tbl =
    StringHash.to_seq_keys tbl
    |> Seq.iter (add_if_unused cf_loc)
  in
  LocHash.iter add_if_unused ctors_fields.declarations;
  res
