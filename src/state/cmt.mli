val read : string -> (Cmt_format.cmt_infos, string) Result.t

val cached_cmti : string -> Cmt_format.cmt_infos option

val cached_cmt : string -> Cmt_format.cmt_infos option

val set_cache_size : int -> unit

val print_cache_stats : unit -> unit
