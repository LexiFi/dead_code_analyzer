# dead_code_analyzer

## Table of contents

- [Introduction](#introduction)
- [Usage](#usage)
    + [Options](#options-and-reports)
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

### Options and reports

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

## Footnotes
