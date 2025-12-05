# Table of contents

+ [Usage](#usage)
+ [Main reporting options](#main-reporting-options)
    + [Global options](#global-options)
    + [Default reports](#default-reports)
    + [Optional arguments](#optional-arguments)
    + [Stylistic issues](#stylistic-issues)
+ [Advanced usage](#advanced-usage)
    + [Path-related options](#path-related-options)
        + [`--exclude <path>`](#--exclude-path)
        + [`--references <path>`](#--references-path)
    + [Value-related options](#value-related-options)
        + [`--underscore`](#--underscore)
        + [`--internal`](#--internal)
    + [Missing or invalid reports](#missing-or-invalid-reports)
        + [`--verbose`](#--verbose)
    + [Thresholds](#thresholds)
        + [Thresholds on default reports](#thresholds-on-default-reports)
        + [Thresholds on optional arguments](#thresholds-on-optional-arguments)

# Usage

Calling `dead_code_analyzer --help` provides the following output, describing
the main command line aspect, different options available and their effects.

The `<path>` argument is any number of directory, `.cmt` and `.cmti` files.
These files can be produced using the compiler flags `-keep-locs` (on by default)
for `.cmti` and `-bin-annot` for `.cmt`.
The directories are traversed looking for such files.
> [!TIP]
> If you are using `dune` for your project, the files can be obtained via the
> [`@check` alias](https://dune.readthedocs.io/en/stable/reference/aliases/check.html)

> [!IMPORTANT]
> The order of the command line arguments matter.\
> Using `dead_code_analyzer <path> <options>` ignores the `<options>` when
> analyzing `<path>`.\
> Similarly, some options may override the effects of
> others. E.g. `dead_code_analyzer --nothing --all <path>` is the same as
> `dead_code_analyzer --all <path>`.

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

# Main reporting options

## Global options

**options :** `--nothing`, `--all`

To ease the usage of the tool to only focus on some kinds of reports, 2 options
are available :
- `--nothing` deactivates all the reports. One can then selectively activate the
  ones they are interested in using the corresponding options (see the sections below).
  This is equivalent to `-E nothing -M nothing -T nothing -On nothing -Oa nothing -S -all`.
- `--all` activates all the reports. One can then selectively deactivate the
  ones they are not interested in using the corresponding options (see the sections below).
  This is equivalent to `-E all -M all -T all -On all -Oa all -S +all`.

Activating or deactivating a report section actually activates or deactivates
some parts of the analysis. Therefore, it impacts the cost (memory and time)
of the analysis.

The `dead_code_analyzer`'s command line arguments are processed and applied
in the order they appear.
Consequently, those 2 options are generally expected to appear before all the
other report-related options. Using them after would simply override the effects
of the others.


**Examples :**
1. `dead_code_analyzer --nothing <options> <path>` deactivates all the reports
   and then apply the `<options>` before analyzing `<path>`.
1. `dead_code_analyzer <options> --nothing <path>` apply the `<options>` and
   then decativates all the reports before analyzing `<path>`, which is not very
   effective.

## Default reports

**options :** `-E <display>`, `-M <display>`, `-T <display>`

By default the `dead_code_analyzer` only looks for unused exported values,
unused methods, and unused constructors and record fields. They can respectively
be deactivated using `-E nothing`, `-M nothing`, or `-T nothing`.

**Example :** `dead_code_analyzer -M nothing -T nothing` will only analyze and
report unused exported values.

Similarly to `--nothing` and `--all`, one can selectivey activate the reports
they are interested in by using respectively `-E all`, `-M all`, or `-T all`.

**Example :** `dead_code_analyzer --nothing -E all` will analyze and report
unused exported values.

> [!Note]
> `dead_code_analyzer <path>` is equivalent to
> `dead_code_analyzer -E all -M all -T all <path>`.

> [!Warning]
> Analyzing unused methods can be memory intensive.

## Optional arguments

**options :** `-On <display>`, `-Oa <display>`

As discussed in [the introduction](USER_DOC.md#introduction), another important
focus of the `dead_code_analyzer` is on the optional arguments never or always used.
Analyzing and reporting on those can be turned on using respectively `-On all`, or
`-Oa all`. Similarly with the previous options, turning these reports off is
done using the `nothing` argument instead of `all`: respectively `-On nothing`,
or `-Oa nothing`. Using `--nothing` or `--all` also affects these sections.

**Example :** `dead_code_analyzer --nothing -On all` will enable the analysis of
optional arguments and only report those that are never used.

> [!Note]
> Reporting for either the optional arguments never or always used actually
> relies on the same analysis which can be memory intensive.

## Stylistic issues

**option :** `-S <warning>`

Finally, as a bonus, one can activate some stylistic reports by using `-S +all`.
Notice the extra `+` before `all`. The `-S` option expects slightly different
arguments from the previous options. Rather than expecting `all` or `nothing`,
it expects a description of the desired stylistic issues to report, with `+`
indicating a category to activate and `-` one to deactivate.
Using `--nothing` or `--all` also affects this section.

**Example :** `dead_code_analyzer --nothing -S +all-bind` only reports stylistic issues,
and reports all of them but the "useless binding" ones

> [!Note]
> `-S +all` is equivalent to `-S +bind+opt+seq+unit`, and `-S -all` to
> `-S -bind-opt-seq-unit`

# Advanced usage

## Path-related options

Imagine a file tree like the one below, with `foo.*` and `lib/*` the actual code
of the product, and `debug/*` code intended for debugging purpose only during
the development.
```
src
├── debug
│   ├── debug.cmti
│   ├── debug.cmt
│   └── debug.ml
├── foo.cmti
├── foo.cmt
├── foo.ml
├── foo.mli
└── lib
    ├── lib.cmti
    ├── lib.cmt
    ├── lib.ml
    └── lib.mli
```

### `--exclude <path>`

`dead_code_analyzer src` will analyze and find unused elements of code in all
of `src`. Some reports may point to locations in `src/debug/debug.ml`, adding
noise because this module is actually used occasionnaly but no reference to it
should subsist in production code.

To ignore that module, both for declarations and uses, one can use the
`--exclude <path>` option.

`dead_code_analyzer --exclude src/debug src` only reports locations outside of
`src/debug`.

### `--references <path>`

Using the previous command line, some reported elements (e.g. located in
`src/lib/lib.mli`) could be used by the `Debug` module. In this situation,
completely excluding `src/debug` from the analysis leads to false
positives (FP): invalid reports.

To fix the situation, one can use the `--references <path>` option. This option
includes the `<path>` when observing uses.

As a result, `dead_code_analyzer --exclude src/debug --references src/debug src`
only reports elements outside of `src/debug` and does not report elements used by
code in `src/debug`.

## Value-related options

### `--underscore`

The compiler ignores unused values when their names are prefixed with an
underscore (e.g. `let _x = ...`). The `dead_code_analyzer` imitates that
behavior. One can enable the analysis and reports on such names using the
`--underscore` option.

> [!Note]
> This behavior (ignoring names starting with underscore) is not only limited
> to unused exported values but applied to all the report kinds. Thus, they are
> all affected by the `--underscore` option.

### `--internal`

The compiler already warns the user about unused values that are not exported.
That is, values that are not exposed in the signature of a module. In case no
interface is available, all the toplevel declaration are exported. Consequently,
none of them can be reported by the compiler. In order to complement the
compiler well, and fit some coding habits, the `dead_code_analyzer` keeps track
of internal uses in implementations (`.ml`) without corresponding interfaces (`.mli`).

Using the `--internal` option activates the analysis of internal use for
implementations that have a corresponding interface file, similarly to
implementations without interface as described above. This can be useful to
reduce the volume of reports. It also expands the notion of unused exported
values from unused externally (can be removed from the `.mli`) to not used at
all (can be removed from the codebase).

**Examples :**
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
   By using the `--internal` option, the `dead_code_analyzer` will not report `f`
   as unused anymore.

## Missing or invalid reports

### `--verbose`

When using the `dead_code_analyzer`, one may encounter false negatives (FN):
missing reports. Users may also encounter false positives (FP): invlaid reports.
In both cases, chances are that some files were not analyzed.
An easy way to see the list of analyzed files is by using the `--verbose` option.
This will print out `Scanning <file_path>` for every file it tries to read.
In case there is an issue reading the file, then it prints out a second line
indicating the `<file_path>` and the issue. In this case, the analyzer ignores
the file and moves on.

- If a file is ignored and it is not obvious why, then opening an issue is
welcome.
- If no file is ignored, check that no file is missing. There should be a `.cmti`
  and a `.cmt` file for each expected `<file_path>`.
- If no `.cmti` or `.cmt` file is missing then the false negatives must be due
  to limitations of the tool and opening an issue is welcome.

## Thresholds

### Thresholds on default reports

**options :** `-E <display>`, `-M <display>`, `-T <display>`

In addition to activating/deactivating some reports, one can extend the details
of the reports using thresholds. The idea with thresholds is to not only report
unused elements of code but also report almost unused ones, up to a desired
maximum amount: the threshold.

For the 3 default reports (exported values, methods, and constructors and record
fields), the threshold argument's format is `threshold:<int>`, with `<int>` a
positive integer. Using this argument will not only enable the corresponding
report but also some subreports for values used exactly `n` times with
`0 < n <= <int>`.

**Example :** `dead_code_analyzer --nothing -E threshold:2` will only analyze
exported values and report those never used, used exactly once, and used exactly
twice.

> [!Note]
> The use of an element of code is defined by its syntactic use. Consequently,
> a function could actually be called mutliple times dynamically but always at
> the same location in code and therefore be reported as used exactly once.

The threshold argument has an ever more detailed variant : `calls:<int>`. This
time, the subreports will not only indicate the elements used exactly `n` times
but also display the locations of their use.

### Thresholds on optional arguments

**options :** `-On <display>`, `-Oa <display>`

The options for optional arguments always/never used also accept a `threshold`
argument. However it looks a bit different from the one for the default reports.
Instead of using `threshold:<int>`, there are 2 different modes available:
`percent:<float>` and `both:<int>,<float>` with `0.0 <= <float> <= 1.0` and
`0 <= <int>`.

Using the `percent:<float>` argument will activate the reports for optional arguments
that are used/unused at least the provided percent of the time.

**Examples :**
1. `dead_code_analyzer --nothing -Oa percent:0.9` will report optional arguments
   always used, and those used at least 90% of the time.
2. `dead_code_analyzer --nothing -On percent:0.9` will report optional arguments
   never used, and those discarded at least 90% of the time.

Using the `both:<int>,<float>` argument will activate the reports for optional
arguments that are always/never used, with at most `<int>` number of exceptions
and at least the provided percent of the time.

**Example :** `dead_code_analyzer --nothing -Oa both:1,0.9` will report optional
arguments always used, and those unused at most once but used at least 90% of
the time.

> [!Tip]
> Using `0` for `<float>` provides the same effect as the `threshold:<int>`
> argument for the default reports.
>
> **Example :** `dead_code_analyzer --nothing -Oa both:1,0` will report optional
> arguments always used, and those unused at most once.

Similarly to the default reports, it is possible to get the callsites of the
exceptions. This is done by prefixing the threshold argument with `calls:`.

**Example :** `dead_code_analyzer --nothing -Oa calls:both:1,0` will report optional
arguments always used, and those unused at most once with the locations where
they are discarded.

