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

let r:t =
  {
    unused = '_';
    f = fun ?a ?b c -> c;
  }

let () = r.f ~a:0 ()

type t2 =
  {
    r: t
  }

let r = {r = r}

let () = r.r.f ~a:0 ()

module X=struct
  type t =
    {
      unused: char;
    }
end
