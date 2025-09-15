# dead_code_analyzer

## Table of contents

- [Introduction](#introduction)
- [Usage](#usage)
    + [Main options](#main-reporting-options)
    + [Advanced usage](#advanced-usage)
- [Footnotes](#footnotes)

## Introduction

The `dead_code_analyzer`'s main goal is to help developers maintain their OCaml
codebase by identifying unused elements of code. Such elements can then be
dropped, either from the interface or from the implementation, depending on the
context. It is a static analyzer and an addition to the compiler's already
existing warnings.

An element of code is considered unused if there is no explicit use of
it[^fixpoint]. This means that it should be removable from the codebase without
breaking compilation[^breaking_compilation].
It implies that each reported element can be taken care of independently.
It also implies that removing an element reported may lead to the detection of
new unused elements.

The unused elements tracked are:
- Exported values
- Constructors and record fields
- Class/object fields (methods and variables)

In addition to those unused elements, the `dead_code_analyzer` also tracks the
use of optional arguments. More specifically, it focuses on those:
- Always used
- Never used (or only to be discarded with `None`)

Finally, as a bonus, it also identifies some stylistic issues. Although this is
not related to the _use_ of elements of code.

[^fixpoint]: There is no fixpoint construction that would imply that the uses of
  an element by another element, which is itself unused, should be discarded.
  I.e. an element only used by unused elements is still considered used.
  \
  E.g. A function `f` called by another function `g` is always considered used,
       no matter the status of `g`.

[^breaking_compilation]: This is not always true. E.g. removing a constructor
  from a variant type would trigger compilation errors on patterns matching that
  constructor.

## Usage

Calling `dead_code_analyzer --help` provides the following output, describing
the main command line aspect, different options available and their effects.

The `<path>` argument is any number of directroym `.cmt` and `.cmi` files.
These files can be obtained using the compiler flags `-keep-locs` (on by
default) for `.cmi` and `-bin-annot` for `.cmt`.
The directories are traversed looking for such files.

```
Usage: dead_code_analyzer <options> <path>
Options are:
  --exclude <path>  Exclude given path from research.
  --references <path>  Consider given path to collect references.
  --underscore  Show names starting with an underscore
  --verbose  Verbose mode (ie., show scanned files)
  -v  See --verbose
  --internal  Keep internal uses as exported values uses when the interface is given. This is the default behaviour when only the implementation is found
  --nothing  Disable all warnings
  -a  See --nothing
  --all  Enable all warnings
  -A  See --all
  -E <display>  Enable/Disable unused exported values warnings.
    <display> can be:
        all
        nothing
        "threshold:<integer>": report elements used up to the given integer
        "calls:<integer>": like threshold + show call sites
  -M <display>  Enable/Disable unused methods warnings.
    See option -E for the syntax of <display>
  -Oa <display>  Enable/Disable optional arguments always used warnings.
    <display> can be:
        all
        nothing
        <threshold>
        "calls:<threshold>" like <threshold> + show call sites
    <threshold> can be:
        "both:<integer>,<float>": both the number max of exceptions (given through the integer) and the percent of valid cases (given as a float) mu
st be respected for the element to be reported
        "percent:<float>": percent of valid cases to be reported
  -On <display>  Enable/Disable optional arguments never used warnings.
    See option -Oa for the syntax of <display>
  -S  Enable/Disable coding style warnings.
    Delimiters '+' and '-' determine if the following option is to enable or disable.
    Options (can be used together):
        bind: useless binding
        opt: optional arg in arg
        seq: use sequence
        unit: unit pattern
        all: bind & opt & seq & unit
  -T <display>  Enable/Disable unused constructors/records fields warnings.
    See option -E for the syntax of <display>
  -help  Display this list of options
  --help  Display this list of options
```

### Main reporting options

#### Default reports

By default the `dead_code_analyzer` only looks for unused exported values,
unused methods, and unused constructors and record fields. They can respectively
be deactivated using `-E nothing`, `-M nothing`, or `-T nothing`.
\
E.g. `dead_code_analyzer -M nothing -T nothing <path>` will only track and
report unused exported values.
> [!Note]
> Tracking unused methods can be memory intensive.

Another way to deactivate these reports is also by using `--nothing`, which
actually indicates the `dead_code_analyzer` to not track anything, effectively
turning off all the reports. Then one can selectivey reactivate the reports they
are interested in by using respectively `-E all`, `-M all`, or `-T all`.
\
E.g. `dead_code_analyzer --nothing -E all` will only track and report unused
exported values.

Oppositely to `--nothing`, there is a `--all` option which indicates the
`dead_code_anlyzer` to track and report everything (advanced usage excluded).

#### Optional arguments

As discussed in the introduction, another important focus of the
`dead_code_analyzer` is on the optional arguments never or always used. Tracking
and reporting on those can be turned on using respectively `-On all`, or
`-Oa all`. Similarly with the previous options, turning those reports off is
done using `--nothing` or th `nothing` argument instead of `all`: respectively
`-On nothing`, or `-Oa nothing`.
\
E.g. `dead_code_analyzer --nothing -On all` will enable the tracking of the use
of optional arguments and only report those that are never used.
> [!Note]
> Enabling the reporting for either the optional arguments never or always
> used activates the same tracking of their use which can be memory intensive.

#### Stylistic issues

Finally, as a bonus, on can activate some stylistic reports by using `-S +all`.
Notice the extra `+` before `all`. The `-S` option expects slightly different
arguments than the previous options. Rather than expecting `all` or `nothing`
(or some other argument described in the advanced usage section), it expects a
description of the desired stylistic issues to report, with `+` indicating a
category to activate and `-` one to deactivate.
\
E.g. `dead_code_analyzer --nothing -S +all-bind` only reports stylistic issues,
and reports all of them but the "useless binding" ones
> [!Note]
> `-S +all` is equivalent to `-S +bind+opt+seq+unit`, and `-S -all` to
> `-S -bind-opt-seq-unit`

### Advanced usage

#### Path-related options

Imagine a file tree as such:
```
src
├── debug
│   ├── debug.cmi
│   ├── debug.cmt
│   └── debug.ml
├── foo.cmi
├── foo.cmt
├── foo.ml
├── foo.mli
└── lib
    ├── lib.cmi
    ├── lib.cmt
    ├── lib.ml
    └── lib.mli
```

**`--exclude <path>`**

One may use `dead_code_analyzer --nothing -E all src` to try and find unused
exported values. The reported values point to locations in `src/debug/debug.ml`,
adding noise because this module is actually used occasionnaly but no reference
to it should subsist in prduction code.

To ignore that module, both for declarations and uses, one can use the
`--exclude <path>` option.

Now calling `dead_code_analyzer --nothing -E all --exlude src/debug src` only
reports values outside of `src/debug`. This is great. However, some reported
values are located in `src/lib/lib.mli` and the `Debug` module actually makes
use of them. Completely excluding `src/debug` from the analysis lead to false
positives (values wrongly report unused).

**`--references <path>`**

To fix the situation, one can use the `--references <path>` option. This option
excludes the `<path>` when gathering declarations but uses to observe uses.

As a result, calling
`dead_code_analyzer --nothing -E all --references src/debug src` does not
report values in `src/debug/debug.ml` anymore and does not report values in
`src/lib/lib.mli` that are used by the `Debug` module.

#### Value-related options

**`--underscore`**

The compiler ignores unused values when their names are prefixed with an
underscore. The `dead_code_analyzer` imitates that behavior. One can
activate the tracking and reports on such names using the `--underscore` option.

**`--internal`**

The compiler already warns the user about unused values that are not exported.
That is, values that are not exposed in the interface, or those that are defined
at the top level. In case no interface is available, no toplevel declaration can
be declared unused by the compiler because they are all exported. In order to
complement the compiler well, and fit some coding habits, the
`dead_code_analyzer` keeps track of internal use in implementations without
corresponding interfaces.
\
E.g. If `foo.ml` defines a function `f` that is only used internally and no
`foo.mli` is provided, then neither the compiler nor the `dead_code_analyzer`
will report it. However, if `f` is not used externally nor internally, then
the `dead_code_analyzer` will report it, while the compiler will not.

Using the `--internal` option activates keeping track of internal use for
implementations that have a corresponding interface file, similarly to
implementations without interface as described above. This can be useful to
reduce the volume of reports. It also expands to notion of unused exported
values from unused externally (can be removed from the `.mli`) to not used at
all (can be removed from the codebase).

#### Missing reports

**`--verbose`**

When using the `dead_code_analyzer`, on may encounter false negatives: values
that should be reported but are not. In this case, chances are that some files
were not analyzed. An easy way to see the list of analyzed files is by using
the `--verbose` option. This will print out either `Scanning <file_path>` if
everything's fine or `Ignoring <file_path> (no source?)` when the analyzer does
not file the source file (`.ml` or `.mli`) corresponding to the current
compiled file (`.cmt` or `.cmti`).

- If a file is ignored and it is not obvious why, then opening an issue is
welcome.
- If no file is ignored, then the false negatives must be due to limitations of
the tool and opening an issue is welcome.

#### Thresholds

In addition to activating/deactivating some reports, one can extend the details
of the reports using thresholds. The idea with thresholds is to not only report
unused elements of code but also reports almost unused ones, up to a desired
maximum amount: the threshold.

For the 3 default reports (exported values, methods, and constructors and record
fields), the threshold argument's format is `threshold:<int>`, with `<int>` a
positive integer. Using this argument will not only enable the corresponding
report but also some subreports for values used exactly `n` times with
`0 < n <= <int>`.
\
E.g. `dead_code_analyzer --nothing -E threshold:2` will only track the use of
exported values and reports those never used, those used only once and those
used only twice.
> [!Note]
> The use of an element of code is defined by its syntactic use. Consequently,
> a function could actually be called mutliple times dynamically but always at
> the same location in code and therefore be reported as used only once.

The threshold argument has an ever more detailed variant : `calls:<int>`. This
time, the subreports will not only indicate the elements only used `n` times
but also display the locations where they are used.

##### Thresholds on optional arguments

Optional arguments always/never used options also accept a `threshold` argument.
However it looks a bit different from the one for the default reports.
Instead of using `threshold:<int>`, there are 2 different modes available:
`percent:<float>` and `both:<int>,<float>` with `0.0 <= <float> <= 1.0` and
`0 <= <int>`.

Using the `percent:<float>` argument will activate the reports for values that
are used/unused at least the provided percent of the time.
\
E.g. `dead_code_analyzer --nothing -Oa percent:0.9` will report optional
arguments always used, and those used at least 90% of the time.

Using the `both:<int>,<float>` argument will activate the reports for values
that are always/never used, with at most `<int>` number of exceptions and at
least the provided percent of the time.
\
E.g. `dead_code_analyzer --nothing -Oa both:1,0.9` will report optional
arguments always used, and those unused at most once but used at least 90% of
the time.
> [!Note]
> Using `0` for `<float>` provides the same effect as the `threshold:<int>`
> argument for the default reports.
> \
> E.g. `dead_code_analyzer --nothing -Oa both:1,0` will report optional
> arguments always used, and those unused at most once.

Similarly to the default reports, it is possible to explicit the callsites of
the exceptions. This is done by prefixinf the threshold argument with `calls:`.
\
E.g. `dead_code_analyzer --nothing -Oa calls:both:1,0` will report optional
arguments always used, and those unused at most once and print the locations
where they are discarded.


## Footnotes
