let () =
  Use_values.mark_used ();
  Use_values_no_intf.mark_used ();
  Use_values_in_submodules.mark_used ();
  Use_values_in_submodules_no_intf.mark_used ()

let () =
  Use_constructors.mark_used ();
  Use_records.mark_used ()

let () =
  Use_opt_args.mark_used ();
  Use_mixed_args.mark_used ();
  Use_opt_args_in_higher_order_fun.mark_used ()

let () =
  Use_without_class.mark_used ();
  Use_with_class.mark_used ()

let () =
  Use_style.mark_used ()
