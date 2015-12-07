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


let string_cut c s =
  let rec loop c s pos len =
    if len = String.length s then s
    else if s.[pos] = c then String.sub s (pos - len) len
    else loop c s (pos + 1) (len + 1)
  in loop c s 0 0


type threshold = {exceptions: int; percentage: float; optional: [`Percent | `Both]}


type opt = {print: bool; call_sites: bool; threshold: threshold}
let opta = ref
  {
    print = false;
    call_sites = false;
    threshold =
      {
        exceptions = 0;
        percentage = 1.;
        optional = `Percent
      };
  }
let optn = ref
  {
    print = false;
    call_sites = false;
    threshold =
      {
        exceptions = 0;
        percentage = 1.;
        optional = `Percent
      };
  }


let update_opt opt s =
  let threshold s =
    if String.sub s 0 5 = "both:" then begin
      let limits = String.sub s 5 (String.length s - 5) in
      let thr = string_cut ',' limits in
      let pos = String.length thr + 1 in
      let pct = String.sub limits pos (String.length limits - pos) in
      opt := {!opt with threshold={!opt.threshold with optional = `Both}};
      let thr = String.trim thr in
      let pct = String.trim pct in
      try
        opt := {!opt with threshold={!opt.threshold with exceptions = int_of_string thr}};
        opt := {!opt with threshold = {!opt.threshold with percentage = float_of_string pct}}
      with Failure _ -> raise (Arg.Bad ("-Ox: wrong arguments: " ^ limits))
    end
    else if String.sub s 0 8 = "percent:" then
      let pct = String.sub s 8 (String.length s - 8) |> String.trim in
      try opt := {!opt with threshold={!opt.threshold with percentage = float_of_string pct}}
      with Failure _ -> raise (Arg.Bad ("-Ox: wrong argument: " ^ pct))
    else raise (Arg.Bad ("-Ox: unknown option " ^ s))
  in
  match s with
  | "all" -> opt := {!opt with print = true}
  | "nothing" -> opt := {!opt with print = false}
  | s ->
      let s =
        if String.sub s 0 6 = "calls:" then
          String.sub s 6 (String.length s - 6)
        else s
      in
      threshold s;
      if !opt.threshold.exceptions < 0 then
        raise (Arg.Bad ("-Ox: number of exceptions must be >= 0"))
      else if !opt.threshold.percentage > 1. || !opt.threshold.percentage < 0. then
        raise (Arg.Bad ("-Ox: percentage must be >= 0.0 and <= 1.0"))


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


let update_basic opt flag = function
    | "all" -> flag := {!flag with print = true}
    | "nothing" -> flag := {!flag with print = false}
    | s ->
        let threshold =
          if String.sub s 0 6 = "calls:" then begin
            flag := {!flag with call_sites = true};
            String.sub s 6 (String.length s - 6)
          end
          else if String.sub s 0 10 = "threshold:" then
            String.sub s 10 (String.length s - 10)
          else raise (Arg.Bad (opt ^ ": unknown option: " ^ s))
        in
        let threshold = String.trim threshold |> int_of_string in
        if threshold < 0 then
          raise (Arg.Bad (opt ^ ": integer should be >= 0; Got " ^ string_of_int threshold))
        else flag := {!flag with threshold}


let verbose = ref false
let set_verbose () = verbose := true

(* Print name starting with '_' *)
let underscore = ref true
let set_underscore () = underscore := false

let internal = ref false
let set_internal () = internal := true
