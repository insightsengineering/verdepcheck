#' Create installation proposal using various dependency strategies
#'
#' These functionalities would read local package `DESCRIPTION` file, derive dependencies and
#' create an installation proposal using various strategies for package versions as described below.
#'
#' @rdname deps_installation_proposal
#' @aliases deps_installation_proposal
#'
#' @section strategies:
#' Currently implemented strategies:
#' * `max` - use the greatest version of dependencies pulled from GitHub - i.e. remove any further reference
#' (i.e. `@<commitish>` `#<pr>` or `@*release`) to use default branch (most likely: "main").
#' * `release` - read `"Remotes"` field and for each GitHub type of the package source: overwrite source with
#' CRAN reference (if available) or overwrite any further reference (i.e. `@<commitish>` `#<pr>` or `@*release`)
#' with a tag associated with the latest release. This mimics the behaviour of `@*release` endpoint of `remotes`
#' package but it does not use it because it is not yet supported by `pkgdepends`.
#' * `min` - use the lowest version of dependencies. If no version is specified then the minimal available
#' version is assumed. See [get_min_ver] for details how the minimal version is determined.
#'
#' Any modification is done for _direct_ dependencies. Indirect ones are installed as usual.
#'
#' @note In order to make full use of the package features, it's recommended to provide `"Remotes"` section
#' in the package `DESCRIPTION` file for all dependent packages. Some functions are supported only for package
#' remotes from GitHub.
#'
#' @param path (`string`) path to the package sources
#' @param config (`list`) configuration options. See [`pkgdepends::pkg_config`] for details.
#' If it does not include `library` then temporary directory is used which simulates clean environment
#' without using any pre-installed packages.
#' If it does not include `dependencies` then `TRUE` value is used which means all hard dependencies plus `Suggests`.
#'
#' @returns `pkg_installation_plan` object
#'
#' @seealso [pkgdepends::pkg_installation_proposal]
#'
#' @export
#' @importFrom pkgdepends parse_pkg_ref
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_max_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_max_deps_installation_proposal <- function(path, config = list()) { # nolint
  path <- normalizePath(path)

  if ("dependencies" %nin% names(config)) config$dependencies <- TRUE
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  d <- desc::desc(path)
  new_remotes <- vapply(
    d$get_remotes(),
    function(x) {
      x_parsed <- pkgdepends::parse_pkg_ref(x)
      if (inherits(x_parsed, "remote_ref_github")) {
        sprintf("%s/%s", x_parsed$username, x_parsed$repo)
      } else {
        x
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
  d <- desc_cond_set_remotes(d, new_remotes)

  res <- desc_to_ip(d, config)
  class(res) <- c("max_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom remotes github_remote
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_release_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_release_deps_installation_proposal <- function(path, config = list()) { # nolint
  path <- normalizePath(path)

  if ("dependencies" %nin% names(config)) config$dependencies <- TRUE
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  d <- desc::desc(path)
  new_remotes <- vapply(
    d$get_remotes(),
    function(x) {
      x_parsed <- pkgdepends::parse_pkg_ref(x)
      if (check_if_on_cran(x_parsed)) {
        x_parsed$package
      } else if (inherits(x_parsed, "remote_ref_github")) {
        release_ref <- remotes::github_remote(sprintf("%s/%s@*release", x_parsed$username, x_parsed$repo))$ref
        sprintf("%s/%s@%s", x_parsed$username, x_parsed$repo, release_ref)
      } else {
        x
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
  d <- desc_cond_set_remotes(d, new_remotes)

  res <- desc_to_ip(d, config)
  class(res) <- c("release_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends new_pkg_deps parse_pkg_ref
#' @importFrom utils installed.packages
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_min_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_min_deps_installation_proposal <- function(path, config = list()) { # nolint
  path <- normalizePath(path)

  if ("dependencies" %nin% names(config)) config$dependencies <- TRUE
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  x <- pkgdepends::new_pkg_deps(
    refs = path,
    config = config,
    policy = "lazy"
  )
  x$solve()
  x$stop_for_solution_error()
  deps <- x$get_solution()$data$deps[[1]]

  deps <- deps[deps$package %nin% c("R", rownames(utils::installed.packages(priority = "base"))), ]

  deps$ref_parsed <- lapply(deps$ref, pkgdepends::parse_pkg_ref)

  deps$ref_minver <- mapply( # @TODO: add cli progress bar
    get_min_ver_incl_cran,
    remote_ref = deps$ref_parsed,
    op = deps$op,
    op_ver = deps$version,
    SIMPLIFY = FALSE
  )

  refs <- vapply(deps$ref_minver, `[[`, character(1), "ref")

  d <- desc::desc(path)
  d <- desc_cond_set_remotes(d, refs)

  res <- desc_to_ip(d, config)
  class(res) <- c("min_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' Set `"Remotes"` section into the `desc` object if not empty else clear `"Remotes"` section.
#' @keywords internal
desc_cond_set_remotes <- function(desc, remotes) {
  if (length(remotes)) {
    desc$set_remotes(remotes)
  } else {
    desc$clear_remotes()
  }
  return(invisible(desc))
}

#' Create `installation_plan` object from `desc` object
#' @importFrom pkgdepends new_pkg_deps new_pkg_installation_proposal
#' @keywords internal
desc_to_ip <- function(desc, config) {
  temp_desc <- tempfile()
  desc$write(temp_desc)

  pkgdepends::new_pkg_installation_proposal(
    refs = paste0("deps::", temp_desc),
    config = config
  )
}
