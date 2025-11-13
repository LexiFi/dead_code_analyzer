let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let blue = "\x1b[34m"
let white = "\x1b[37m"
let bg_red = "\x1b[41m"
let style_reset = "\x1b[0m"
let bold = "\x1b[01m"

let error ~err ~ctx =
  if Out_channel.isatty Out_channel.stderr then
    Printf.eprintf "%s%s: %s%s%s%s\n%!" red ctx white bg_red err style_reset
  else Printf.eprintf "%s: %s\n%!" ctx err
