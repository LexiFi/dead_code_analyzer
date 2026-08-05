# Table of contents

+ [Module type](#module-type)
+ [Module type of](#module-type-of)

The limitations listed below apply to all the reportable elements of code.
Coding style issues are not concerned unless specified otherwise.

# Module type

Related issue :
[issue #50](https://github.com/LexiFi/dead_code_analyzer/issues/50).

As explained in the
[exported values' Module type](./exported_values/code_constructs/MODTYP.md)
example, the analyzer is currently restrcited to not reporting values declared
in module types. This is actually true for all reportable elements of code.
This means that any unused element defined by a module with a module type as
signature, even with constraints or substitutions, will not be reported.

A future improvement would be to report unused elements declared in module types
by considering all the elemnts defined in modules of such types as instances
of the elements in the module types.

Examples of this limitation are avaialble in the
[modtype](../../examples/docs/limitations/modtype) directory.

# Module type of

The same restricition as for the [Module type](#module-type) limitation above
is applied to modules whose module types are recovered via `module type of`.
Using this construction makes the module behave similarly to a module type, so
the module type semantics apply to it.

Examples of this limitation are avaialble in the
[modtype_of](../../examples/docs/limitations/modtype_of) directory.
