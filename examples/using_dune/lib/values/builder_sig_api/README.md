This is a pattern observed on Frama-C.

A `module_api.ml` defines the module types, `module.ml` implements modules of
those module type and `module.mli` creates the link by exposing modules in the
implmentation with types from the api.
Additionally, there may be a `module_builder.ml` (and `.mli`) which defines
functors (in particular `Make`) using module types from the api.

An example of this pattern can be found in the Frama-C project in
`src/kernel_services/ast_printing/` with `module = printer`.
A variation of this example, with a `module_sig.ml` for the api  can be found in
`src/kernel_services/cmdline_parameters/`. This time `module = parameter`
