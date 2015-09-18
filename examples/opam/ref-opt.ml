let f ?a ?(b = 12) x = x

let no x = x + 12

let r = ref f

let g = f

let () = r := g

let x =  !r ~a:1 0
