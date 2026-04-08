# Table of contents

+ [Coding style](#coding-style)
    + [Usage](#usage)

# Coding style

This section relates to coding patterns. Although this is not related to dead
code, the tool has been reporting style issues almost since its conception
(commit [0adbc0d](https://github.com/LexiFi/dead_code_analyzer/commit/0adbc0d504830a01d64a701572b5e7cb3b29f03e)).
Tracking stylistic issues comes for "free" during the analysis and can be handy
when cleaning up a codebase so it was kept as part of the tool.

The analyzer only reports 4 different kinds of stylistic issues :
- `bind`: useless binding
- `opt`: parameter of arrow type expecting an optional argument
- `seq`: binding to unit instead of using sequence
- `unit`: binding unit to a name

## Usage

Stylistic issues are not reported by default.
Their reports can be activated by using the `--all` or `-S +all` command line
arguments.
They can be deactivated by using the `--nothing` or `-S -all` command line
arguments.
Each of the sylistic issue category can be selectively activated/decativated as
described in their respective sections.
For more details about the command line arguments see [the more general Usage
documentation](../USAGE.md).

The report section looks like:
```
.> CODING STYLE:
===============
filepath:line: issue

Nothing else to report in this section
--------------------------------------------------------------------------------
```
The report line format is `filepath:line: issue` with `filepath` the absolute
path to the file where the `issue` lies, `line` the line index in `filepath` at
which the `issue` is, and `issue` the description of the stylistic issue.
There can be any number of such lines.

The expected resolution depends on the reported issue category. This is
described in their respective sections.
