let r = ref (fun ?a ?b () -> 0)

let x = !r () + 12
