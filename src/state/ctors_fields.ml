type t = {
  declarations : string Utils.StringHash.t Utils.LocHash.t;
    (** location of decl -> builddir -> cf_path *)
  uses : Utils.LocSet.t Utils.LocHash.t;
    (** cf_loc -> use_locs *)
  equivalences : string Utils.StringHash.t;
    (** t1_path -> t2_path => t1 = t2 *)
  locations : Lexing.position Utils.StringHash.t;
    (** cf_path -> cf_loc | type_path -> type_loc
        This is useful for implicit equivalences (e.g. via include) and for
        local aliases.
    *)
  components : Lexing.position Utils.StringHash.t Utils.LocHash.t;
    (** type_loc -> cf_name -> cf_loc *)
}

let create () =
  let open Utils in
  let declarations = LocHash.create 256 in
  let uses = LocHash.create 256 in
  let equivalences = StringHash.create 64 in
  let locations = StringHash.create 256 in
  let components = LocHash.create 256 in
  {declarations; uses; equivalences; locations; components}

let add_component ~type_loc ~cf_name ~cf_loc ctors_fields =
  let open Utils in
  let cf_tbl =
    match LocHash.find_opt ctors_fields.components type_loc with
    | Some tbl -> tbl
    | None ->
        let tbl = StringHash.create 4 in
        LocHash.add ctors_fields.components type_loc tbl;
        tbl
  in
  StringHash.replace cf_tbl cf_name cf_loc;
  ctors_fields

let find_component ~type_loc ~cf_name ctors_fields =
  let open Utils in
  let cf_tbl = LocHash.find_opt ctors_fields.components type_loc in
  Option.bind cf_tbl (fun cf_tbl -> StringHash.find_opt cf_tbl cf_name)

let add_loc_binding ~path ~loc ctors_fields =
  Utils.StringHash.add ctors_fields.locations path loc;
  ctors_fields

let find_loc ~path ctors_fields =
  Utils.StringHash.find_opt ctors_fields.locations path

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

let get_exported_declarations ctors_fields =
  let open Utils in
  LocHash.fold
    (fun cf_loc builddir_tbl acc ->
      StringHash.fold
        (fun builddir cf_path acc -> (cf_loc, builddir, cf_path)::acc)
        builddir_tbl
        acc
    )
    ctors_fields.declarations
    []

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

let add_equivalence ~t1_path ~t2_path ctors_fields =
  Utils.StringHash.add ctors_fields.equivalences t1_path t2_path;
  ctors_fields

let resolve_equivalences ctors_fields =
  let open Utils in
  (* implement a pseudo union-find *)
  (* reprs points to another member of the location's equivalence class.
     This member was the representative at some point. There are no
     circular references.
     _The_ representative of a class points to itself.
     Use get_repr to get _the_ representative of a location's class.
  *)
  let reprs = LocHash.create 128 in
  let rec get_repr loc =
    (* explore members of loc's class until finding the class representative *)
    match LocHash.find_opt reprs loc with
    | None ->
        (* loc does not belong to a class yet. Setup its own *)
        LocHash.add reprs loc loc;
        loc
    | Some repr when repr = loc -> loc (* class representative found *)
    | Some repr ->  get_repr repr (* class member but not the representative *)
  in
  let join_classes t1_path t2_path =
    let type_loc1 = find_loc ~path:t1_path ctors_fields in
    let type_loc2 = find_loc ~path:t2_path ctors_fields in
    match type_loc1, type_loc2 with
    | None, _ | _, None -> ()
    | Some type_loc1, Some type_loc2 ->
        let type_repr1 = get_repr type_loc1 in
        let type_repr2 = get_repr type_loc2 in
        LocHash.replace reprs type_repr1 type_repr2
  in
  let copy_references_to_repr type_loc =
    let type_repr = get_repr type_loc in
    LocHash.find_opt ctors_fields.components type_repr
    |> Option.iter (fun cf_tbl ->
        StringHash.iter
          (fun cf_name cf_repr ->
            find_component ~type_loc ~cf_name ctors_fields
            |> Option.iter (fun cf_loc ->
                get_uses ~cf_loc ctors_fields
                |> List.iter (fun use_loc -> add_use ~cf_loc:cf_repr ~use_loc ctors_fields |> ignore))
          )
          cf_tbl)
  in
  let copy_references_from_repr type_loc =
    let type_repr = get_repr type_loc in
    LocHash.find_opt ctors_fields.components type_repr
    |> Option.iter (fun cf_tbl ->
          StringHash.iter
            (fun cf_name cf_repr ->
              find_component ~type_loc ~cf_name ctors_fields
              |> Option.iter (fun cf_loc ->
                  get_uses ~cf_loc:cf_repr ctors_fields
                  |> List.iter (fun use_loc -> add_use ~cf_loc ~use_loc ctors_fields |> ignore))
            )
            cf_tbl)
  in
  let copy_references_to_repr t1_path _ =
    find_loc ~path:t1_path ctors_fields
    |> Option.iter copy_references_to_repr
  in
  let copy_references_from_repr t1_path _ =
    find_loc ~path:t1_path ctors_fields
    |> Option.iter copy_references_from_repr
  in
  StringHash.iter join_classes ctors_fields.equivalences;
  StringHash.iter copy_references_to_repr ctors_fields.equivalences;
  StringHash.iter copy_references_from_repr ctors_fields.equivalences;
  ctors_fields
