type v = Used | Unused

type v2 = Int of int | Float of float | Nan | Nothing

val f: [`Value of int (* Should be detected ? *)| `Unit] -> v2
