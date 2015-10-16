type t0 =
  {
    unused: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }

type t =
  {
    unused: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }

type t2 =
  {
    r: t
  }
