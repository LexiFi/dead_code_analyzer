type t0 =
  {
    unused: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }

module X:sig
  type t =
    {
      unused: char;
      used: char;
      mutable f: (?a:int -> ?b:int -> unit -> unit);
    }
end

type t2 =
  {
    r: X.t
  }

type u = X.t =
  {
    unused: char;
    used: char;
    mutable f: (?a:int -> ?b:int -> unit -> unit);
  }
