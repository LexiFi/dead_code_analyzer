type cmi_cmt_infos = Cmi_format.cmi_infos option * Cmt_format.cmt_infos

val read : string -> (cmi_cmt_infos, string) Result.t

val cached_cmti : string -> cmi_cmt_infos option

val cached_cmt : string -> cmi_cmt_infos option

val print_cache_stats : unit -> unit
