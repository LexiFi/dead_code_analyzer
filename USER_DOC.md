# dead_code_analyzer

## Table of contents

- [Introduction](#introduction)
- [Usage](#usage)
    + [Main options](#main-reporting-options)
    + [Advanced usage](#advanced-usage)
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
- Constructors and record fields
- Class/object fields (methods and variables)

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

## Usage

Calling `dead_code_analyzer --help` provides the following output, describing
the main command line aspect, different options available and their effects.

The `<path>` argument is any number of directory, `.cmt` and `.cmi` files.
These files can be produced using the compiler flags `-keep-locs` (on by default)
for `.cmi` and `-bin-annot` for `.cmt`.
The directories are traversed looking for such files.
> [!TIP]
> If you are using `dune` for your project, the files can be obtained via the
> [`@check` alias](https://dune.readthedocs.io/en/stable/reference/aliases/check.html)

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

Another way to deactivate these reports is by using `--nothing`, which
actually indicates the `dead_code_analyzer` to not track anything, effectively
turning off all the reports. Then one can selectivey reactivate the reports they
are interested in by using respectively `-E all`, `-M all`, or `-T all`.
\
E.g. `dead_code_analyzer --nothing -E all` will only track and report unused
exported values.

Oppositely to `--nothing`, there is a `--all` option which indicates the
`dead_code_anlyzer` to track and report everything ([advanced usage](#advanced-usage) excluded).

#### Optional arguments

As discussed in the introduction, another important focus of the
`dead_code_analyzer` is on the optional arguments never or always used. Tracking
and reporting on those can be turned on using respectively `-On all`, or
`-Oa all`. Similarly with the previous options, turning those reports off is
done using `--nothing` or the `nothing` argument instead of `all`: respectively
`-On nothing`, or `-Oa nothing`.
\
E.g. `dead_code_analyzer --nothing -On all` will enable the tracking of the use
of optional arguments and only report those that are never used.
> [!Note]
> Enabling the reporting for either the optional arguments never or always
> used activates the same analysis which can be memory intensive.

#### Stylistic issues

Finally, as a bonus, on can activate some stylistic reports by using `-S +all`.
Notice the extra `+` before `all`. The `-S` option expects slightly different
arguments from the previous options. Rather than expecting `all` or `nothing`,
it expects a description of the desired stylistic issues to report, with `+`
indicating a category to activate and `-` one to deactivate.
\
E.g. `dead_code_analyzer --nothing -S +all-bind` only reports stylistic issues,
and reports all of them but the "useless binding" ones
> [!Note]
> `-S +all` is equivalent to `-S +bind+opt+seq+unit`, and `-S -all` to
> `-S -bind-opt-seq-unit`

### Advanced usage

#### Path-related options

Imagine a file tree like the one below, with `foo.*` and `lib/*` the actual code
of the product, and `debug/*` code intended for debugging purpose only during
the development.
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
exported values. Some reported values point to locations in `src/debug/debug.ml`,
adding noise because this module is actually used occasionnaly but no reference
to it should subsist in production code.

To ignore that module, both for declarations and uses, one can use the
`--exclude <path>` option.

Now calling `dead_code_analyzer --nothing -E all --exclude src/debug src` only
reports values outside of `src/debug`. This is great! However, some reported
values are located in `src/lib/lib.mli` and the `Debug` module actually makes
use of them. Completely excluding `src/debug` from the analysis leads to false
positives (FP): invalid reports.

**`--references <path>`**

To fix the situation, one can use the `--references <path>` option. This option
includes the `<path>` when observing uses.

As a result, calling
`dead_code_analyzer --nothing -E all --exclude src/debug --references src/debug src`
does not report values in `src/debug/debug.ml` and does not report values in
`src/lib/lib.mli` that are used by the `Debug` module.

#### Value-related options

**`--underscore`**

The compiler ignores unused values when their names are prefixed with an
underscorei (e.g. `let _x = ...`). The `dead_code_analyzer` imitates that behavior. One can
activate the tracking and reports on such names using the `--underscore` option.

**`--internal`**

The compiler already warns the user about unused values that are not exported.
That is, values that are not exposed in the signature of a module. In case no
interface is available, all the toplevel declaration are exported. Consequently,
none of them can be reported unused by the compiler. In order to complement the
compiler well, and fit some coding habits, the `dead_code_analyzer` keeps track
of internal uses in implementations (`.ml`) without corresponding interfaces (`.mli`).

Using the `--internal` option activates keeping track of internal use for
implementations that have a corresponding interface file, similarly to
implementations without interface as described above. This can be useful to
reduce the volume of reports. It also expands the notion of unused exported
values from unused externally (can be removed from the `.mli`) to not used at
all (can be removed from the codebase).

E.g.
1. `f` is exported and used internally. Neither the compiler nor the
   `dead_code_analyzer` will report it.
    ```ocaml
    (* foo.ml *)
    (* foo.mli does not exist *)
    let f x = x
    let () = f ()
    ```
2. `f` is exported and not used internally. Assuming it is not used externally
   either, the `dead_code_analyzer` will report it, while the compiler will not.
    ```ocaml
    (* foo.ml *)
    (* foo.mli does not exist *)
    let f x = x
    ```
3. `f` is exported via `foo.mli` and used internally. Assuming it is not used
   externally, the `dead_code_analyzer` will report it by default.
    ```ocaml
    (* foo.ml *)
    let f x = x
    let () = f ()
    ```
    ```ocaml
    (* foo.mli *)
    val f : 'a -> 'a
    ```
    Using the `--internal` option, the `dead_code_analyzer` will not report `f`
    as unused anymore.

#### Missing reports

**`--verbose`**

When using the `dead_code_analyzer`, on may encounter false negatives (FN):
missing reports. In this case, chances are that some files were not analyzed.
An easy way to see the list of analyzed files is by using the `--verbose` option.
This will print out `Scanning <file_path>` for every file it tries to read.
In case there is an issue reading the file, then it prints out a second line
indicating the `<file_path>` and the issue. In this case, the analyzer ignores
the file and moves on.

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
exported values and report those never used, used exactly once, and used exactly
twice.
> [!Note]
> The use of an element of code is defined by its syntactic use. Consequently,
> a function could actually be called mutliple times dynamically but always at
> the same location in code and therefore be reported as used exactly once.

The threshold argument has an ever more detailed variant : `calls:<int>`. This
time, the subreports will not only indicate the elements used exactly `n` times
but also display the locations where they are used.

##### Thresholds on optional arguments

Optional arguments always/never used options also accept a `threshold` argument.
However it looks a bit different from the one for the default reports.
Instead of using `threshold:<int>`, there are 2 different modes available:
`percent:<float>` and `both:<int>,<float>` with `0.0 <= <float> <= 1.0` and
`0 <= <int>`.

Using the `percent:<float>` argument will activate the reports for optional arguments
that are used/unused at least the provided percent of the time.
\
E.g.
1. `dead_code_analyzer --nothing -Oa percent:0.9` will report optional arguments
   always used, and those used at least 90% of the time.
2. `dead_code_analyzer --nothing -On percent:0.9` will report optional arguments
   never used, and those discarded at least 90% of the time.

Using the `both:<int>,<float>` argument will activate the reports for optional
arguments that are always/never used, with at most `<int>` number of exceptions
and at least the provided percent of the time.
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

Similarly to the default reports, it is possible to get the callsites of the
exceptions. This is done by prefixing the threshold argument with `calls:`.
\
E.g. `dead_code_analyzer --nothing -Oa calls:both:1,0` will report optional
arguments always used, and those unused at most once with the locations where
they are discarded.


## Footnotes
