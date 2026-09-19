type t = {
  declarations : string Utils.StringHash.t Utils.LocHash.t;
    (** location of decl -> builddir -> val_path *)
  uses : Utils.LocSet.t Utils.LocHash.t
}

let create () =
  let open Utils in
  let declarations = LocHash.create 256 in
  let uses = LocHash.create 256 in
  {declarations; uses}

let add_exported_declaration ~builddir ~val_loc ~val_path vals =
  let open Utils in
  let tbl =
    match LocHash.find_opt vals.declarations val_loc with
    | Some tbl -> tbl
    | None ->
        let tbl = StringHash.create 8 in
        LocHash.add vals.declarations val_loc tbl;
        tbl
  in
  (* Collisions at the same val_loc in the same builddir should not happen.
      TODO: confirm [x] and [y] have different locs in [let x as y = ...] *)
  StringHash.replace tbl builddir val_path;
  vals

let remove_exported_declaration ~builddir ~val_loc vals =
  let open Utils in
  match LocHash.find_opt vals.declarations val_loc with
  | None -> vals
  | Some tbl ->
      StringHash.remove tbl builddir;
      vals

let is_exported_declaration ~val_loc vals =
  Utils.LocHash.mem vals.declarations val_loc

let get_val_path ~builddir ~val_loc vals =
  let open Utils in
  match LocHash.find_opt vals.declarations val_loc with
  | None -> None
  | Some tbl -> StringHash.find_opt tbl builddir

let add_use ~val_loc ~use_loc vals =
  let open Utils in
  LocHash.find_set vals.uses val_loc
  |> LocSet.add use_loc
  |> LocHash.replace vals.uses val_loc;
  vals

let remove_uses ~val_loc vals =
  Utils.LocHash.remove vals.uses val_loc;
  vals

let get_uses ~val_loc vals =
  let open Utils in
  LocHash.find_set vals.uses val_loc
  |> LocSet.to_seq
  |> List.of_seq

let add_alias ~orig_loc ~alias_loc vals =
  let open Utils in
  let alias_uses = LocHash.find_set vals.uses alias_loc in
  let orig_uses = LocHash.find_set vals.uses orig_loc in
  let orig_uses = LocSet.union alias_uses orig_uses in
  LocHash.replace vals.uses orig_loc orig_uses;
  vals

let get_unused ?(max_uses=0) vals =
  let open Utils in
  let res = Hashtbl.create (max_uses + 1) in
  let add_if_unused val_loc builddir =
    let nb_uses = LocSet.cardinal (LocHash.find_set vals.uses val_loc) in
    if nb_uses <= max_uses then
      let locs =
        Hashtbl.find_opt res nb_uses
        |> Option.value ~default:[]
      in
      let locs = (val_loc, builddir)::locs in
      Hashtbl.replace res nb_uses locs
  in
  let add_if_unused val_loc tbl =
    StringHash.to_seq_keys tbl
    |> Seq.iter (add_if_unused val_loc)
  in
  LocHash.iter add_if_unused vals.declarations;
  res
