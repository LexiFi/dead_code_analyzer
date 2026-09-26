type 'a name_to_a = 'a Utils.StringHash.t

type definition =
  | Defined
  | Override of string list (* overriden_paths *)
  | Inherited of string * string list (* inherited_path, overriden_paths *)
  | Virtual

type t = {
  declarations : string name_to_a name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> builddir -> meth_path *)
  uses : Utils.LocSet.t name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> use_locs *)
  self_uses : Utils.LocSet.t name_to_a Utils.LocHash.t;
    (** obj_loc -> meth_name -> use_locs
        Same as {!uses} above but for uses of [meth_name] of [obj_loc]
        within the object/class definition.
        In particular, this is useful for inheritances, to propagate self
        uses to the actual (parent or child) method definition.
    *)
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
  path_aliases : string Utils.StringHash.t;
    (** local_path -> global_path
        This is used to refer to the fully qualified version of a locally
        defined object/class.
        This is particularly useful for included definitions, which are
        relocated within the module where the include happen, to point to
        the actual definitions in conjunction with {!locations} above.
    *)
}

let create () =
  let open Utils in
  let declarations = LocHash.create 128 in
  let uses = LocHash.create 128 in
  let self_uses = LocHash.create 32 in
  let definitions = LocHash.create 128 in
  let aliases = LocHash.create 128 in
  let locations = StringHash.create 128 in
  let path_aliases = StringHash.create 128 in
  {declarations; uses; self_uses; definitions; aliases; locations; path_aliases}

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

let add_path_alias ~orig_path ~alias_path meths =
  Utils.StringHash.add meths.path_aliases alias_path orig_path;
  meths

let find_orig_path ~obj_path meths =
  Utils.StringHash.find_opt meths.path_aliases obj_path

let get_orig_path ~obj_path meths =
  find_orig_path ~obj_path meths
  |> Option.value ~default:obj_path

let reset_path_aliases meths =
  Utils.StringHash.reset meths.path_aliases;
  meths

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

let add_any_use ~obj_loc ~meth_name ~use_loc use_tbl =
  (* [use_tbl] is either [meths.uses] or [meths.self_uses]
     XXX: using either as parameter relies on the fact that we use
          hashtables for storage.
  *)
  let open Utils in
  let meth_tbl =
    find_meth_tbl_or_default use_tbl obj_loc ~default_size:8
  in
  let use_set =
    match StringHash.find_opt meth_tbl meth_name with
    | Some set -> set
    | None -> LocSet.empty
  in
  let use_set = LocSet.add use_loc use_set in
  StringHash.replace meth_tbl meth_name use_set

let add_use ~obj_loc ~meth_name ~use_loc meths =
  add_any_use ~obj_loc ~meth_name ~use_loc meths.uses;
  meths

let add_self_use ~obj_loc ~meth_name ~use_loc meths =
  add_any_use ~obj_loc ~meth_name ~use_loc meths.self_uses;
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

let get_any_uses ~obj_loc ~meth_name use_tbl =
  (* [use_tbl] is either [meths.uses] or [meths.self_uses]
     XXX: using either as parameter relies on the fact that we use
          hashtables for storage.
  *)
  let open Utils in
  LocHash.find_opt use_tbl obj_loc
  |> Option.map
    (fun meth_tbl ->
      match StringHash.find_opt meth_tbl meth_name with
      | Some use_set -> LocSet.to_seq use_set |> List.of_seq
      | None -> []
    )
  |> Option.value ~default:[]

let get_uses ~obj_loc ~meth_name meths =
  get_any_uses ~obj_loc ~meth_name meths.uses

let find_definition ~builddir ~obj_loc ~meth_name meths =
  let open Utils in
  let ( let* ) x f = Option.bind x f in
  let* meth_tbl = LocHash.find_opt meths.definitions obj_loc in
  let* builddir_tbl = StringHash.find_opt meth_tbl meth_name in
  StringHash.find_opt builddir_tbl builddir

let mark_defined ~builddir ~obj_loc ~meth_name meths =
  let definition =
    match find_definition ~builddir ~obj_loc ~meth_name meths with
    | Some (Inherited (path, overriden_paths)) ->
       Override (path::overriden_paths)
    | Some (Override _ as override) -> override
    | _ -> Defined
  in
  add_definition ~builddir ~obj_loc ~meth_name ~definition meths

let mark_inherited ~builddir ~obj_loc ~meth_name ~inherited_path meths =
  let definition =
    match find_definition ~builddir ~obj_loc ~meth_name meths with
    | Some (Inherited (path, overriden_paths)) ->
       Inherited (inherited_path, path::overriden_paths)
    | Some (Override overriden_paths) ->
       Inherited (inherited_path, overriden_paths)
    | _ -> Inherited (inherited_path, [])
  in
  add_definition ~builddir ~obj_loc ~meth_name ~definition meths

let mark_virtual ~builddir ~obj_loc ~meth_name meths =
  add_definition ~builddir ~obj_loc ~meth_name ~definition:Virtual meths

let add_initializer ~builddir ~obj_loc meths =
  (* An initializer is a special hidden method. *)
  mark_defined ~builddir ~obj_loc ~meth_name:"!!initializer!!" meths

let inherit_initializer ~builddir ~obj_loc ~inherited_path meths =
  (* An initializer is a special hidden method. *)
  mark_inherited ~builddir ~obj_loc
    ~meth_name:"!!initializer!!" ~inherited_path meths

let is_marked_defined ~builddir ~obj_loc ~meth_name meths =
    find_definition ~builddir ~obj_loc ~meth_name meths
    |> Option.map (function
      | Defined | Override _ -> true
      | _ -> false)
    |> Option.value ~default:false

let add_alias ~orig_loc ~alias_loc meths =
  Utils.LocHash.replace meths.aliases alias_loc orig_loc;
  meths

let copy_uses ~orig_loc ~alias_loc ~meth_name src_tbl dst_tbl =
  (* [src_tbl] and [dst_tbl] are either [meths.uses] or [meths.self_uses]
     XXX: using either as parameter relies on the fact that we use
          hashtables for storage.
  *)
  let open Utils in
  let get_uses use_tbl loc =
    match LocHash.find_opt use_tbl loc with
    | None -> LocSet.empty
    | Some meth_tbl ->
        match StringHash.find_opt meth_tbl meth_name with
        | None -> LocSet.empty
        | Some use_set -> use_set
  in
  let alias_uses = get_uses src_tbl alias_loc in
  let orig_uses = get_uses dst_tbl orig_loc in
  let orig_uses = LocSet.union alias_uses orig_uses in
  let meth_tbl =
    find_meth_tbl_or_default dst_tbl orig_loc ~default_size:8
  in
  StringHash.replace meth_tbl meth_name orig_uses

let resolve_aliases meths =
  let open Utils in
  let copy_uses ~alias_loc ~orig_loc use_tbl =
    match LocHash.find_opt use_tbl alias_loc with
    | None -> meths
    | Some alias_meth_tbl ->
        StringHash.iter
          (fun meth_name _alias_uses ->
            copy_uses ~orig_loc ~alias_loc ~meth_name use_tbl use_tbl
          )
          alias_meth_tbl;
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
      copy_uses ~alias_loc ~orig_loc meths.uses
      |> (fun meths -> copy_uses ~alias_loc ~orig_loc meths.self_uses)
      |> move_definitions ~alias_loc ~orig_loc
      |> remove_exported_declarations ~obj_loc:alias_loc
    )
    meths.aliases
    meths

let resolve_inheritances meths =
  (* TODO: fix-point resolution: the order in which methods are explored
     traversed is undefined. Because, we copy uses between direct parent
     and child, uses may not be propagated to a grand-parent or grand-child
     if we propagate from parent to child before grand-parent to parent,
     or from parent to grand-parent before child to parent.
 *)
  let open Utils in
  let propagate propagate_fun meths =
    (* apply propagate_fun on methods represented by their obj_loc,
       meth_name, and definition *)
    let seen = LocHash.create 128 in
    LocHash.iter
      (fun obj_loc meth_tbl ->
        let obj_loc = get_orig_loc ~obj_loc meths in
        let propagate_fun = propagate_fun ~obj_loc in
        if LocHash.mem seen obj_loc then ()
        else begin
          LocHash.add seen obj_loc ();
          StringHash.iter
            (fun meth_name builddir_tbl ->
              let propagate_fun = propagate_fun ~meth_name in
              StringHash.iter
                (fun _builddir definition ->
                  propagate_fun ~definition meths
                )
                builddir_tbl
            )
            meth_tbl
        end
      )
      meths.definitions;
    meths
  in
  let find_orig_parent_loc ~obj_path meths =
    find_loc ~obj_path meths
    |> Option.map (fun obj_loc -> get_orig_loc ~obj_loc meths)
  in
  let propagate_downward meths =
    let copy_uses ~obj_loc ~parent_loc meths =
      (* propagate self uses from all the parent's methods *)
      LocHash.find_opt meths.self_uses parent_loc
      |> Option.iter
          (fun meth_tbl ->
            StringHash.iter
              (fun meth_name _ ->
                copy_uses ~alias_loc:parent_loc ~orig_loc:obj_loc
                  ~meth_name meths.self_uses meths.uses;
                copy_uses ~alias_loc:parent_loc ~orig_loc:obj_loc
                  ~meth_name meths.self_uses meths.self_uses
              )
              meth_tbl
          )
    in
    let propagate_from_parents ~obj_loc =
      let seen = StringHash.create 8 in
      fun ~paths meths ->
        List.iter
          (fun parent_path ->
            if StringHash.mem seen parent_path then ()
            else begin
              StringHash.add seen parent_path ();
              find_orig_parent_loc ~obj_path:parent_path meths
              |> Option.iter (fun parent_loc ->
                  copy_uses ~obj_loc ~parent_loc meths)
            end
          )
          paths
    in
    propagate
      (fun ~obj_loc ->
        let propagate_from_parents = propagate_from_parents ~obj_loc in
        fun ~meth_name:_ ~definition meths ->
        match definition with
        | Override paths ->
            propagate_from_parents ~paths meths
        | Inherited (inherited_path, overriden_paths) ->
            let paths = inherited_path::overriden_paths in
            propagate_from_parents ~paths meths
        | _ -> ()
      )
      meths
  in
  let propagate_upward meths =
    propagate
      (fun ~obj_loc ~meth_name ~definition meths ->
        match definition with
        | Inherited (inherited_path, _) ->
            (* propagate all the uses upward *)
            find_orig_parent_loc ~obj_path:inherited_path meths
            |> Option.iter (fun parent_loc ->
                copy_uses ~alias_loc:obj_loc ~orig_loc:parent_loc
                  ~meth_name meths.uses meths.uses;
                remove_exported_declaration ~obj_loc ~meth_name meths
                |> ignore)
        | _ -> ()
      )
      meths
  in
  propagate_downward meths
  |> propagate_upward

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
