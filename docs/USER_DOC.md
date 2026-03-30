# dead_code_analyzer

## Table of contents

- [Introduction](#introduction)
- [Topics](#topics)
- [Footnotes](#footnotes)

## Introduction

The `dead_code_analyzer`'s main goal is to help developers maintain their OCaml
codebase by identifying globally unused elements of code. Such elements can then
be dropped, either from the interface or from the implementation, depending on
the context. It is a static analyzer and an addition to the compiler's already
existing warnings.

An element of code is considered unused if there is no explicit use of
it. This means that it should be removable from the codebase without changing
the semantics of the program nor breaking compilation[^breaking_compilation].
This "_unused_" property is not considered transitively[^on_transitivity].
It implies that each reported element can be taken care of independently.
It also implies that removing an element reported may lead to the detection of
new unused elements.

The unused elements tracked are:
- Exported values
- Methods
- Constructors and record fields

In addition to those unused elements, the `dead_code_analyzer` also tracks the
use of optional arguments. More specifically, it focuses on those:
- Always used
- Never used (or only to be discarded with `None`)

Finally, as a bonus, it also identifies some stylistic issues. Although this is
not related to the _use_ of elements of code.

[^breaking_compilation]: Actually, the compilation may break in some cases.
  E.g. removing a constructor from a variant type would trigger compilation
  errors on patterns matching that constructor.

[^on_transitivity]: Elements of code used by an unused element of code are
  still considered used.
  Consequently, an element only used by unused elements is still considered
  used. It would only be reported as unused after all its users have been
  removed from the codebase.
  \
  E.g. A function `f` called by another function `g` is always considered used,
       no matter the status of `g`.

## Topics

This documentation is split accross different topics:

- [Usage](USAGE.md) describes the usage of the `dead_code_analyzer` and its options.
- [Exported Values](exported_values/EXPORTED_VALUES.md) describes the semantics and usage of the "unused exported values" report section, and provides examples.
- [Methods](methods/METHODS.md) describes the semantics and usage of the "unused methods" report section, and provides examples.
- [Constructors/Record fields](fields_and_constructors/FIELDS_AND_CONSTRUCTORS.md) describes the semantics and usage of the "unused constructors/record fields" report section, and provides examples.


## Footnotes
