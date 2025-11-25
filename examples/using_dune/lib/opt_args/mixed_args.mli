val interlaced_mandatory_and_opt :
  'a -> ?never:'b -> 'c -> ?sometimes:'d -> 'e -> ?always:'f -> 'g -> unit
val interlaced_labeled_and_opt :
  lab1:'a ->
  ?never:'b ->
  lab2:'c -> ?sometimes:'d -> lab3:'e -> ?always:'f -> unit -> unit
val interlaced_mandatory_labeled_and_opt :
  lab1:'a ->
  ?never:'b -> 'c -> ?sometimes:'d -> lab2:'e -> ?always:'f -> unit -> unit
