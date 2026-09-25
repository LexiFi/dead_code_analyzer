type 'a name_to_a = 'a Utils.StringHash.t

type definition =
  | Defined
  | Inherited of string (* inherited_path *)
  | Virtual

type t = {
  declarations : string name_to_a name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> builddir -> meth_path *)
  uses : Utils.LocSet.t name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> use_loc *)
  definitions : definition name_to_a name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> builddir -> definition
        This is used to keep track of methods whose definition belong
        to the obj_loc and those that are inherited or virtual.
        Only defined methods are reported.
    *)
  aliases : Lexing.position Utils.LocHash.t;
    (** alias_loc -> orig_loc
        NOTE: an alias may be an instance of the class declared at orig_loc
    *)
  locations: Lexing.position Utils.StringHash.t;
    (** obj_path -> obj_loc
        This is used to retrieve the location information from a path.
        In particular, this is useful when inheritances happen because
        a Tcf_inherit knows the shape and name of the inherited class but
        not the location of its definition or declaration.
    *)
}

let create () =
  let open Utils in
  let declarations = LocHash.create 128 in
  let uses = LocHash.create 128 in
  let definitions = LocHash.create 128 in
  let aliases = LocHash.create 128 in
  let locations = StringHash.create 128 in
  {declarations; uses; definitions; aliases; locations}

let get_orig_loc ~obj_loc meths =
  let open Utils in
  let rec get_orig ~prev_locs ~obj_loc =
    (* prev_locs protects against circular references *)
    match LocHash.find_opt meths.aliases obj_loc with
    | None -> obj_loc
    | Some next_loc when LocSet.mem next_loc prev_locs -> obj_loc
    | Some next_loc ->
        let prev_locs = LocSet.add obj_loc prev_locs in
        get_orig ~prev_locs ~obj_loc:next_loc
  in
  get_orig ~prev_locs:LocSet.empty ~obj_loc

let add_loc_binding ~obj_path ~obj_loc meths =
  Utils.StringHash.add meths.locations obj_path obj_loc;
  meths

let find_loc ~obj_path meths =
  Utils.StringHash.find_opt meths.locations obj_path

let find_meth_tbl_or_default tbl ~default_size key =
  let open Utils in
  match LocHash.find_opt tbl key with
  | Some meth_tbl -> meth_tbl
  | None ->
      let meth_tbl = StringHash.create default_size in
      LocHash.add tbl key meth_tbl;
      meth_tbl

let find_builddir_tbl_or_default tbl ~default_size key =
  (* TODO: factorize:
      same as find_meth_tbl_or_default on StringHash instead of LocHash
  *)
  let open Utils in
  match StringHash.find_opt tbl key with
  | Some builddir_tbl -> builddir_tbl
  | None ->
      let builddir_tbl = StringHash.create default_size in
      StringHash.add tbl key builddir_tbl;
      builddir_tbl

let add_definition ~builddir ~obj_loc ~meth_name ~definition meths =
  let open Utils in
  let meth_tbl =
    find_meth_tbl_or_default meths.definitions obj_loc ~default_size:8
  in
  let builddir_tbl =
    find_builddir_tbl_or_default meth_tbl meth_name ~default_size:4
  in
  (* Ignore collisions *)
  StringHash.replace builddir_tbl builddir definition;
  meths

let add_exported_declaration ~builddir ~obj_loc ~meth_name ~meth_path meths =
  let open Utils in
  let meth_tbl =
    find_meth_tbl_or_default meths.declarations obj_loc ~default_size:8
  in
  let builddir_tbl =
    find_builddir_tbl_or_default meth_tbl meth_name ~default_size:4
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

let mark_defined ~builddir ~obj_loc ~meth_name meths =
  add_definition ~builddir ~obj_loc ~meth_name
    ~definition:Defined meths

let mark_inherited ~builddir ~obj_loc ~meth_name ~inherited_path meths =
  add_definition ~builddir ~obj_loc ~meth_name
    ~definition:(Inherited inherited_path) meths

let mark_virtual ~builddir ~obj_loc ~meth_name meths =
  add_definition ~builddir ~obj_loc ~meth_name
    ~definition:Virtual meths

let is_marked_defined ~builddir ~obj_loc ~meth_name meths =
  let open Utils in
  let ( let* ) x f = Option.bind x f in
  let is_defined =
    let* meth_tbl = LocHash.find_opt meths.definitions obj_loc in
    let* builddir_tbl = StringHash.find_opt meth_tbl meth_name in
    let* definition = StringHash.find_opt builddir_tbl builddir in
    Some (definition = Defined)
  in
  Option.value ~default:false is_defined

let add_alias ~orig_loc ~alias_loc meths =
  Utils.LocHash.replace meths.aliases alias_loc orig_loc;
  meths

let copy_uses ~orig_loc ~alias_loc ~meth_name meths =
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

let resolve_aliases meths =
  let open Utils in
  let copy_uses ~alias_loc ~orig_loc meths =
    match LocHash.find_opt meths.uses alias_loc with
    | None -> meths
    | Some alias_use_tbl ->
        StringHash.fold
          (fun meth_name _alias_uses meths ->
            copy_uses ~orig_loc ~alias_loc ~meth_name meths
          )
          alias_use_tbl
          meths
  in
  let move_definitions ~alias_loc ~orig_loc meths =
    match LocHash.find_opt meths.definitions alias_loc with
    | None -> meths
    | Some meth_tbl ->
        let meths =
          StringHash.fold
            (fun meth_name builddir_tbl meths ->
              StringHash.fold
                (fun builddir definition meths ->
                  add_definition ~builddir ~obj_loc:orig_loc ~meth_name
                    ~definition meths
                )
                builddir_tbl
                meths
            )
            meth_tbl
            meths
        in
        LocHash.remove meths.definitions alias_loc;
        meths
  in
  LocHash.fold
    (fun alias_loc _ meths ->
      let orig_loc = get_orig_loc ~obj_loc:alias_loc meths in
      copy_uses ~alias_loc ~orig_loc meths
      |> move_definitions ~alias_loc ~orig_loc
      |> remove_exported_declarations ~obj_loc:alias_loc
    )
    meths.aliases
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
    |> Seq.iter
      (fun builddir ->
        if is_marked_defined ~builddir ~obj_loc ~meth_name meths then
          add_if_unused obj_loc meth_name builddir
      )
  in
  let add_if_unused obj_loc meth_tbl =
    StringHash.iter (add_if_unused obj_loc) meth_tbl
  in
  LocHash.iter add_if_unused meths.declarations;
  res
