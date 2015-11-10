(***************************************************************************)
(*                                                                         *)
(**  Copyright (c) 2014-2015 LexiFi SAS. All rights reserved.              *)
(*                                                                         *)
(*   This source code is licensed under the ISC License                    *)
(*   found in the LICENSE file at the root of this source tree             *)
(*                                                                         *)
(***************************************************************************)

let list_of_opt ?(ignore = []) str =
  try
    let rec split acc pos len =
      let jump =
        try
          List.find
          (fun s -> pos >= String.length s && String.sub str (pos - String.length s) (String.length s) = s)
          ignore
        with Not_found -> ""
      in
      if jump <> "" then
        split acc (pos - String.length jump - 1) (len + String.length jump + 1)
      else if str.[pos] <> '+' && str.[pos] <> '-' then
        split acc (pos - 1) (len + 1)
      else let acc = (str.[pos] = '+', String.trim (String.sub str (pos + 1) len)) :: acc in
        if pos > 0 then split acc (pos - 1) 0
        else acc
    in split [] (String.length str - 1) 0
  with _ -> raise (Arg.Bad ("options' arguments must start with a delimiter (`+' or `-')"))


type threshold = {exceptions: int; percentage: float; optional: [`Percent | `Both]}
let threshold = ref
  {
    exceptions = 0;
    percentage = 1.;
    optional = `Percent
  }

let update_threshold s =
  let rec aux l = match l with
    | (_, "both")::l ->
        threshold := {!threshold with optional = `Both};
        aux l
    | (_, "percent")::l ->
        threshold := {!threshold with optional = `Percent};
        aux l
    | (_, s)::l -> begin try
          begin try threshold := {!threshold with exceptions = int_of_string s}
          with Failure _ -> threshold := {!threshold with percentage = float_of_string s} end;
          if !threshold.exceptions < 0 then
            raise (Arg.Bad ("--threshold: number of exceptions must be >= 0"))
          else if !threshold.percentage > 1. || !threshold.percentage < 0. then
            raise (Arg.Bad ("--threshold: percentage must be >= 0.0 and <= 1.0"))
          else aux l
        with Failure _ -> raise (Arg.Bad ("--threshold: unknown option: " ^ s)) end;
    | [] -> ()
  in aux (list_of_opt s)


type opt = {always: bool; never: bool; call_sites: bool}
let opt = ref
  {
    always = false;
    never = false;
    call_sites = false
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


type exported = {print: bool; call_sites: bool}
let exported = ref
  {
    print = true;
    call_sites = false;
  }

let update_exported s =
  let rec aux = function
    | (b, "all")::l -> exported := {!exported with print = b};
        aux l
    | (b, "calls")::l -> opt := {!opt with call_sites = b};
        aux l
    | (_, "")::l -> aux l
    | (_, s)::_ -> raise (Arg.Bad ("-E: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let update_call_sites s =
  let rec aux l = match l with
    | (b, "E")::l -> exported := {!exported with call_sites = b}; aux l
    | (b, "O")::l -> opt := {!opt with call_sites = b}; aux l
    | (b, "all")::l ->
        exported := {!exported with call_sites = b};
        opt := {!opt with call_sites = b};
        aux l
    | (_, s)::_ -> raise (Arg.Bad ("--call-sites: unknown option: " ^ s))
    | [] -> ()
  in aux (list_of_opt s)


let verbose = ref false
let set_verbose () = verbose := true

(* Print name starting with '_' *)
let underscore = ref false
let set_underscore () = underscore := true

let types = ref []
let update_types s = types := !types @ List.map (fun (_, s) -> s) (list_of_opt ~ignore:["->"] s)

let internal = ref false
let set_internal () = internal := true
