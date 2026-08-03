module M_intf = Modtype_with_intf.Ftor ()
module M_intf_with = Modtype_with_intf.Ftor_with ()
module M_intf_subst = Modtype_with_intf.Ftor_subst ()

let () =
  let open Modtype_with_intf in
  With.(f ~always ()) |> ignore;
  Subst.(f ~always ()) |> ignore;
  Incl.(f ~always ()) |> ignore;
  Incl_with.(f ~always ()) |> ignore;
  Incl_subst.f ~always:Incl_subst.always () |> ignore;
  M_intf.(f ~always ()) |> ignore;
  M_intf_with.(f ~always ()) |> ignore;
  M_intf_subst.(f ~always ()) |> ignore

module M_no_intf = Modtype_without_intf.Ftor ()
module M_no_intf_with = Modtype_without_intf.Ftor_with ()
module M_no_intf_subst = Modtype_without_intf.Ftor_subst ()

let () =
  let open Modtype_without_intf in
  With.(f ~always ()) |> ignore;
  Subst.(f ~always ()) |> ignore;
  Incl.(f ~always ()) |> ignore;
  Incl_with.(f ~always ()) |> ignore;
  Incl_subst.f ~always:Incl_subst.always () |> ignore;
  M_no_intf.(f ~always ()) |> ignore;
  M_no_intf_with.(f ~always ()) |> ignore;
  M_no_intf_subst.(f ~always ()) |> ignore
