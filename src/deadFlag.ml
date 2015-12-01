(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

let list_of_opt str =
  try
    let rec split acc pos len =
      if str.[pos] <> '+' && str.[pos] <> '-' then
        split acc (pos - 1) (len + 1)
      else let acc = (str.[pos] = '+', String.trim (String.sub str (pos + 1) len)) :: acc in
        if pos > 0 then split acc (pos - 1) 0
        else acc
    in split [] (String.length str - 1) 0
  with _ -> raise (Arg.Bad ("options' arguments must start with a delimiter (`+' or `-')"))


type threshold = {exceptions: int; percentage: float; optional: [`Percent | `Both]}


type opt = {always: bool; never: bool; call_sites: bool; threshold: threshold}
let opt = ref
  {
    always = false;
    never = false;
    call_sites = false;
    threshold =
      {
        exceptions = 0;
        percentage = 1.;
        optional = `Percent
      };
  }

let update_opt s =
  let rec aux = function
    | (b, "always")::l -> opt := {!opt with always = b};
        aux l
    | (b, "never")::l -> opt := {!opt with never = b};
        aux l
    | (b, "calls")::l -> opt := {!opt with call_sites = b};
        aux l
    | (b, "all")::l -> opt := {!opt with never = b; always = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-O: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let update_opt_threshold s =
  let rec aux l = match l with
    | (_, "both")::l ->
        opt := {!opt with threshold={!opt.threshold with optional = `Both}};
        aux l
    | (_, "percent")::l ->
        opt := {!opt with threshold={!opt.threshold with optional = `Percent}};
        aux l
    | (_, s)::l -> begin try
          begin try opt := {!opt with threshold={!opt.threshold with exceptions = int_of_string s}};
          with Failure _ -> opt := {!opt with threshold = {!opt.threshold with percentage = float_of_string s}} end;
          if !opt.threshold.exceptions < 0 then
            raise (Arg.Bad ("--thresholdO: number of exceptions must be >= 0"))
          else if !opt.threshold.percentage > 1. || !opt.threshold.percentage < 0. then
            raise (Arg.Bad ("--thresholdO: percentage must be >= 0.0 and <= 1.0"))
          else aux l
        with Failure _ -> raise (Arg.Bad ("--thresholdO: unknown option: " ^ s)) end;
    | [] -> ()
  in aux (list_of_opt s)


type style = {opt_arg: bool; unit_pat: bool; seq: bool; binding: bool}
let style = ref
  {
    opt_arg = false;
    unit_pat = false;
    seq = false;
    binding = false;
  }

let update_style s =
  let rec aux = function
    | (b, "opt")::l -> style := {!style with opt_arg = b};
        aux l
    | (b, "unit")::l -> style := {!style with unit_pat = b};
        aux l
    | (b, "seq")::l -> style := {!style with seq = b};
        aux l
    | (b, "bind")::l -> style := {!style with binding = b};
        aux l
    | (b, "all")::l -> style := {unit_pat = b; opt_arg = b; seq = b; binding = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-S: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


type basic = {print: bool; call_sites: bool; threshold: int}
let exported = ref
  {
    print = true;
    call_sites = false;
    threshold = 0
  }


let obj = ref
  {
    print = true;
    call_sites = false;
    threshold = 0;
  }


let typ = ref
  {
    print = true;
    call_sites = false;
    threshold = 0
  }


let update_basic opt flag s =
  let rec aux = function
    | (b, "all")::l -> flag := {!flag with print = b};
        aux l
    | (b, "calls")::l -> flag := {!flag with call_sites = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad (opt ^ ": unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let update_threshold opt (flag : basic ref) x =
  if x < 0 then raise (Arg.Bad (opt ^ ": integer should be >= 0; Got " ^ string_of_int x))
  else flag := {!flag with threshold = x}


let update_call_sites s =
  let update (flag : basic ref) b = flag := {!flag with call_sites = b} in
  let rec aux l = match l with
    | (b, "C")::l -> update obj b; aux l
    | (b, "E")::l -> update exported b; aux l
    | (b, "O")::l -> opt := {!opt with call_sites = b}; aux l
    | (b, "T")::l -> update typ b; aux l
    | (b, "all")::l ->
        update obj b;
        update exported b;
        opt := {!opt with call_sites = b};
        update typ b;
        aux l
    | (_, s)::_ -> raise (Arg.Bad ("--call-sites: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let verbose = ref false
let set_verbose () = verbose := true

(* Print name starting with '_' *)
let underscore = ref true
let set_underscore () = underscore := false

let internal = ref false
let set_internal () = internal := true
