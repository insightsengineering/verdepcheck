# verdepcheck

Have you ever encounter following errors?

> Error: object ‘foo’ is not exported by 'namespace:bar'

> `foo()` was deprecated in bar 1.0.0.
> i Please use `baz()` instead.

This package is a tool for package developers to check your package using various versions of dependencies. It will help you detect new breaking changes of dependencies as well as the minimal version supported.

# Overview

Typical workflow includes the following:

* read local package dependencies from the `DESCRIPTION` file
* derieve dependencies version according to the strategy used
* resolve and identify potential conflicts of dependencies
* download and install to the temporary directory
* execute `R CMD CHECK` using directory from the previous step as a libpath

Supported strategies are:

* `max` - use the greatest (development) versions of dependencies
* `release` - use the latest release - using `@*release` reference of the `remotes` package
* `min` - use the minimal version of dependencies

The main functions are:

* `new_<strategy>_deps_installation_proposal` for creating `installation_proposal` objects
* `<strategy>_deps_check` that creates and executes `installation_proposal` and then run `"R CMD CHECK"`

It is heavily based on `pkgdown` for dependency resolution and `rcmdcheck` for executing `"R CMD CHECK"`.

# Install

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/verdepcheck@*release")
```

# Usage

The main goal of package authors is to use it within GitHubAction or any other CI tool.

```r
x <- max_deps_check("(path to your package)")

x$ip$show_solution()
x$ip$draw()
x$ip$create_lockfile("/path/to/pkg.lock")

x$check$status
x$check$session_info
```
