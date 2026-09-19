type builddir_to_path = string Utils.StringHash.t
type 'a name_to_a = 'a Utils.StringHash.t

type t = {
  declarations : builddir_to_path name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> builddir -> meth_path *)
  uses : Utils.LocSet.t name_to_a Utils.LocHash.t
    (** obj_loc -> meth_name -> use_loc *)
}

let create () =
  let open Utils in
  let declarations = LocHash.create 256 in
  let uses = LocHash.create 256 in
  {declarations; uses}

let find_meth_tbl_or_default tbl ~default_size key =
  let open Utils in
  match LocHash.find_opt tbl key with
  | Some meth_tbl -> meth_tbl
  | None ->
      let meth_tbl = StringHash.create default_size in
      LocHash.add tbl key meth_tbl;
      meth_tbl

let add_exported_declaration ~builddir ~obj_loc ~meth_name ~meth_path meths =
  let open Utils in
  let meth_tbl =
    find_meth_tbl_or_default meths.declarations obj_loc ~default_size:8
  in
  let builddir_tbl =
    (* TODO: factorize:
        same as find_meth_tbl_or_default on StringHash instead of LocHash
    *)
    match StringHash.find_opt meth_tbl meth_name with
    | Some tbl -> tbl
    | None ->
        let tbl = Utils.StringHash.create 4 in
        StringHash.add meth_tbl meth_name tbl;
        tbl
  in
  (* Collisions at the same meth_name at the obj_loc in the same builddir
     should not happen. *)
  StringHash.replace builddir_tbl builddir meth_path;
  meths

let remove_exported_declaration ?builddir ~obj_loc ~meth_name meths =
  let open Utils in
  begin (* actual removal *)
  match LocHash.find_opt meths.declarations obj_loc with
  | None -> ()
  | Some meth_tbl ->
      match builddir with
      | None -> StringHash.remove meth_tbl meth_name
      | Some builddir ->
          match StringHash.find_opt meth_tbl meth_name with
          | None -> ()
          | Some builddir_tbl -> StringHash.remove builddir_tbl builddir
  end;
  meths

let remove_exported_declarations ?builddir ~obj_loc meths =
  let open Utils in
  begin (* actual removal *)
  match builddir with
  | None -> LocHash.remove meths.declarations obj_loc
  | Some builddir ->
      match LocHash.find_opt meths.declarations obj_loc with
      | None -> ()
      | Some meth_tbl ->
          StringHash.fold
            (fun meth_name builddir_tbl clearable_meth ->
              StringHash.remove builddir_tbl builddir;
              if StringHash.length builddir_tbl = 0 then
                meth_name :: clearable_meth
              else clearable_meth
            )
            meth_tbl
            []
          |> List.iter (StringHash.remove meth_tbl)
  end;
  meths

let is_exported_declaration ~obj_loc meths =
  Utils.LocHash.mem meths.declarations obj_loc

let get_meth_path ~builddir ~obj_loc ~meth_name meths =
  let open Utils in
  let meth_tbl = LocHash.find_opt meths.declarations obj_loc in
  let builddir_tbl =
    Option.bind meth_tbl (fun meth_tbl -> StringHash.find_opt meth_tbl meth_name)
  in
  Option.bind builddir_tbl (fun builddir_tbl ->
    StringHash.find_opt builddir_tbl builddir)

let add_use ~obj_loc ~meth_name ~use_loc meths =
  let open Utils in
  let meth_tbl =
    find_meth_tbl_or_default meths.uses obj_loc ~default_size:8
  in
  let use_set =
    match StringHash.find_opt meth_tbl meth_name with
    | Some set -> set
    | None -> LocSet.empty
  in
  let use_set = LocSet.add use_loc use_set in
  StringHash.replace meth_tbl meth_name use_set;
  meths

let remove_uses ~obj_loc ?meth_name meths =
  let open Utils in
  begin (* clear uses *)
    match meth_name with
    | None -> LocHash.remove meths.uses obj_loc
    | Some meth_name ->
        LocHash.find_opt meths.uses obj_loc
        |> Option.iter (fun tbl -> StringHash.remove tbl meth_name)
  end;
  meths

let get_uses ~obj_loc ~meth_name meths =
  let open Utils in
  LocHash.find_opt meths.uses obj_loc
  |> Option.map
    (fun use_tbl ->
      match StringHash.find_opt use_tbl meth_name with
      | Some use_set -> LocSet.to_seq use_set |> List.of_seq
      | None -> []
    )
  |> Option.value ~default:[]

let add_alias ~orig_loc ~alias_loc ~meth_name meths =
  let open Utils in
  let get_uses loc =
    match LocHash.find_opt meths.uses loc with
    | None -> LocSet.empty
    | Some use_tbl ->
        match StringHash.find_opt use_tbl meth_name with
        | None -> LocSet.empty
        | Some use_set -> use_set
  in
  let alias_uses = get_uses alias_loc in
  let orig_uses = get_uses orig_loc in
  let orig_uses = LocSet.union alias_uses orig_uses in
  let use_tbl =
    find_meth_tbl_or_default meths.uses orig_loc ~default_size:8
  in
  StringHash.replace use_tbl meth_name orig_uses;
  meths

let add_alias ~orig_loc ~alias_loc ?meth_name meths =
  let open Utils in
  match meth_name with
  | Some meth_name -> add_alias ~orig_loc ~alias_loc ~meth_name meths
  | None ->
      match LocHash.find_opt meths.uses alias_loc with
      | None -> meths
      | Some alias_use_tbl ->
          StringHash.fold
            (fun meth_name _alias_uses meths ->
              add_alias ~orig_loc ~alias_loc ~meth_name meths
            )
            alias_use_tbl
            meths

let get_unused ?(max_uses=0) meths =
  let open Utils in
  let res = Hashtbl.create (max_uses + 1) in
  let add_if_unused obj_loc meth_name builddir =
    let nb_uses = List.length (get_uses ~obj_loc ~meth_name meths) in
    if nb_uses <= max_uses then
      let locs =
        Hashtbl.find_opt res nb_uses
        |> Option.value ~default:[]
      in
      let locs = (obj_loc, meth_name, builddir)::locs in
      Hashtbl.replace res nb_uses locs
  in
  let add_if_unused obj_loc meth_name builddir_tbl =
    StringHash.to_seq_keys builddir_tbl
    |> Seq.iter (add_if_unused obj_loc meth_name)
  in
  let add_if_unused obj_loc meth_tbl =
    StringHash.iter (add_if_unused obj_loc) meth_tbl
  in
  LocHash.iter add_if_unused meths.declarations;
  res
