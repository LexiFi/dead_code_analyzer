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

module X:sig
  type t =
    {
      unused: char;
    }
end

type u = t0 =
  {
    unused: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }
