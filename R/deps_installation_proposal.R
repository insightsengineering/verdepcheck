#' Create installation proposal using various dependency strategies
#'
#' These functionalities would read local package `DESCRIPTION` file, derive dependencies from
#' `"Config/Needs/verdepcheck"` and create an installation proposal using various strategies for package versions
#' as described below.
#'
#' @rdname deps_installation_proposal
#' @aliases deps_installation_proposal
#'
#' @section strategies:
#' Currently implemented strategies:
#' * `max` - use the greatest version of dependent packages
#' * `release` - use released version of dependent packages - use CRAN if possible else if GitHub release
#' is available then use it else fail.
#' * `min` - use the lowest version of dependent packages incorporating minimal version specification in
#' `"Imports"` and `"Suggests"`. If no version is specified then the minimal available
#' version is assumed. See [get_ref_min] for details how the minimal version is determined.
#'
#' Any modification is done for _direct_ dependencies. Indirect ones are installed as usual.
#'
#' @section configuration:
#' `verdepcheck` will look into `"Config/Needs/verdepcheck"` field of the `DESCRIPTION` file for dependent packages
#' references. See [`pkgdepends::pkg_refs`] for details.
#' Some functions are supported only for package references from GitHub.
#' If you specify additional details (i.e. tag, commit, PR or `@*release`) then it wouldn't be changed. Therefore,
#' in order to make full use of various strategies, it is recommended to specify general reference in form of
#' `[<package>=][github::]<username>/<repository>[/<subdir>]` - i.e. without `[<detail>]` part.
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
#' @importFrom desc desc
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_max_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_max_deps_installation_proposal <- function(path, config = list()) { # nolint
  path <- normalizePath(path)

  config$dependencies <- .desc_field
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  d <- desc::desc(path)
  new_refs <- lapply(get_refs_from_desc(d), get_ref_max)
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")
  d <- desc_cond_set_refs(d, new_refs_str)

  res <- desc_to_ip(d, config)
  class(res) <- c("max_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_release_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_release_deps_installation_proposal <- function(path, config = list()) { # nolint
  path <- normalizePath(path)

  config$dependencies <- .desc_field
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  d <- desc::desc(path)
  new_refs <- lapply(get_refs_from_desc(d), get_ref_release)
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")
  d <- desc_cond_set_refs(d, new_refs_str)

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

  config$dependencies <- .desc_field
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
  for (i in seq_len(nrow(deps))) {
    if (deps[i, "type"] != .desc_field) next
    package <- deps[i, "package"]
    deps[i, "op"] <- deps[deps$package == package, "op"][1]
    deps[i, "version"] <- deps[deps$package == package, "version"][1]
  }
  deps <- deps[deps$type == .desc_field, ]

  deps$ref_parsed <- lapply(deps$ref, pkgdepends::parse_pkg_ref)

  new_refs <- mapply( # @TODO: add cli progress bar
    get_ref_min_incl_cran,
    remote_ref = deps$ref_parsed,
    op = deps$op,
    op_ver = deps$version,
    SIMPLIFY = FALSE
  )
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")

  d <- desc::desc(path)
  d <- desc_cond_set_refs(d, new_refs_str)

  res <- desc_to_ip(d, config)
  class(res) <- c("min_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' Read `"Config/Needs/verdepcheck"` section and return vector of references.
#' @importFrom pkgdepends parse_pkg_ref
#' @keywords internal
get_refs_from_desc <- function(d) {
  if (.desc_field %nin% d$fields()) {
    return(list())
  }
  lapply(
    trimws(strsplit(d$get_field(.desc_field), ",")[[1]]),
    function(x) pkgdepends::parse_pkg_ref(x)
  )
}

#' Set `"Config/Needs/verdepcheck"` section into the `desc` object if not empty else clear this section.
#' @keywords internal
desc_cond_set_refs <- function(d, refs) {
  if (length(refs)) {
    d$set_list(.desc_field, refs)
  } else {
    d$del(.desc_field)
  }
  return(invisible(d))
}

#' Create `installation_plan` object from `desc` object
#' @importFrom pkgdepends new_pkg_deps new_pkg_installation_proposal
#' @keywords internal
desc_to_ip <- function(d, config) {
  temp_desc <- tempfile()
  d$write(temp_desc)

  pkgdepends::new_pkg_installation_proposal(
    refs = paste0("deps::", temp_desc),
    config = config
  )
}
