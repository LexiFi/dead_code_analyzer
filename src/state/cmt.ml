module Cache = struct
  (** O(1) addition and retrieval *)

  type ('k, 'v) t =
    { store : ('k, 'v) Hashtbl.t
        (** filepath -> cmi_cmt_infos *)
    ; mutable capacity : int (** n <= capacity *)
    ; mutable hit : int
    ; mutable miss : int
    }

  let create capacity =
    { store = Hashtbl.create capacity
    ; capacity
    ; hit = 0
    ; miss = 0
    }

  let update_capacity cache capacity =
    cache.capacity <- capacity;
    if Hashtbl.length cache.store >= cache.capacity then
      Hashtbl.reset cache.store

  let find_opt (cache : ('k, 'v) t) (key : 'k) : 'v option =
    let res = Hashtbl.find_opt cache.store key in
    if Option.is_some res then cache.hit <- cache.hit + 1
    else (cache.miss <- cache.miss + 1);
    res

  let add cache key value =
    if Hashtbl.length cache.store = cache.capacity then
      Hashtbl.reset cache.store;
    Hashtbl.replace cache.store key value

end

type cmi_cmt_infos = Cmi_format.cmi_infos option * Cmt_format.cmt_infos

let cache_cmt : ((string * string), (string * cmi_cmt_infos)) Cache.t = Cache.create 64

let set_cache_size capacity = Cache.update_capacity cache_cmt capacity

let print_cache_stats () =
  print_endline (Printf.sprintf "CMT CACHE : hit = %i ; miss = %i"
                                cache_cmt.hit cache_cmt.miss)


let read_no_cache filepath =
  match Cmt_format.read filepath with
  | exception _ -> Result.error (filepath ^ ": error reading file")
  | _, None -> Result.error (filepath ^ ": cmt_infos not found")
  | cmi_infos, Some cmt_infos ->
    Result.ok (cmi_infos, cmt_infos)

let read filepath =
  let comp_unit = Utils.Filepath.unit filepath in
  let ext = Filename.extension filepath in
  match Cache.find_opt cache_cmt (ext, comp_unit) with
  | Some (fp, res) when String.equal fp filepath -> Result.ok res
  | _ ->
      read_no_cache filepath
      |> Result.map (fun cmi_cmt_infos ->
        Cache.add cache_cmt (ext, comp_unit) (filepath, cmi_cmt_infos);
        cmi_cmt_infos)

let find_cached_from_comp_unit comp_unit ext =
  Cache.find_opt cache_cmt (ext, comp_unit)
  |> Option.map snd

let cached_cmti comp_unit =
  find_cached_from_comp_unit comp_unit ".cmti"

let cached_cmt comp_unit =
  find_cached_from_comp_unit comp_unit ".cmt"
