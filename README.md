# verdepcheck

Have you ever encounter following errors?

```r
Error: object ‘foo’ is not exported by 'namespace:bar'
```

```r
`foo()` was deprecated in bar 1.0.0.
i Please use `baz()` instead.
```

This package is a tool for package developers to check your package using various versions of dependencies. It will help you detect new breaking changes of dependencies as well as the minimal version supported.

## Overview

Typical workflow includes the following:

- read local package dependencies from the `DESCRIPTION` file using dedicated `Config/Needs/verdepcheck` field
- derive dependencies version from `Imports` and `Suggests` according to the strategy used
- resolve and identify potential conflicts of dependencies
- download and install to the temporary directory
- execute `R CMD CHECK` using directory from the previous step as a library path

Supported strategies are:

- `max` - use the greatest version of dependent packages. Please note that using development version is not guaranteed to be stable.
- `release` - use the released version of dependent packages. It will try use CRAN if possible else if GitHub release is available then use it else fail.
- `min_cohort` - find maximum date of directly dependent packages release dates and use that as PPM snapshot date for dependency resolve.
- `min_isolated` - for each direct dependency: find its release date and use it as PPM snapshot for resolving itself. Next, combine all the individual resolutions and resolve it altogether again.

The main functions are:

- `new_<strategy>_deps_installation_proposal` for creating `installation_proposal` objects
- `<strategy>_deps_check` that creates and executes `installation_proposal` and then run `"R CMD CHECK"`

This package is heavily based on [`pkgdepends`](https://r-lib.github.io/pkgdepends/) for dependency resolution and [`rcmdcheck`](https://rcmdcheck.r-lib.org/) for executing `"R CMD CHECK"`.

## Install

```r
devtools::install_github("insightsengineering/verdepcheck")
```

## Usage

The main goal of package authors is to use it within GitHub Action or any other CI tool. See [r-verdepcheck-action](https://github.com/insightsengineering/r-verdepcheck-action).

```r
x <- max_deps_check("(path to your package)")

# print results for debugging
x$ip$show_solution()
x$ip$draw()

# create artifact
x$ip$create_lockfile("/path/to/pkg.lock")

# print R CMD CHECK results
x$check$session_info
x$check$status
```
