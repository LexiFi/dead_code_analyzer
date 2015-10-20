type t0 =
  {
    unused: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }

module X=struct
  type t =
    {
      unused: char;
      used: char;
      mutable f: (?a:int -> ?b:int -> unit -> unit);
    }
end

let r:X.t =
  {
    unused = '_';
    used = '_';
    f = fun ?a ?b c -> c;
  }

let () = r.f ~a:0 ()

type t2 =
  {
    r: X.t
  }

let r = {r = r}

let () = r.r.f ~a:0 ()

type u = X.t =
  {
    unused: char;
    used: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }

let r:u =
  {
    unused = '_';
    used = '_';
    f = fun ?a ?b c -> c;
  }

let () = ignore r.used
