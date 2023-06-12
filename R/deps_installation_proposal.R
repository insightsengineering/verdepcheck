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
new_max_deps_installation_proposal <- function( # nolint
                                               path,
                                               config = list(
                                                 dependencies = .desc_field,
                                                 library = tempfile()
                                               )) {
  path <- normalizePath(path)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  new_refs <- list()

  cli_pb_init("max", length(refs))
  for (i in seq_along(refs)) {
    cli_pb_update(refs[[i]]$package)
    new_refs <- c(new_refs, list(get_ref_release(refs[[i]])))
  }
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
new_release_deps_installation_proposal <- function( # nolint
                                                   path,
                                                   config = list(
                                                     dependencies = .desc_field,
                                                     library = tempfile()
                                                   )) {
  path <- normalizePath(path)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  new_refs <- list()

  cli_pb_init("release", length(refs))
  for (i in seq_along(refs)) {
    cli_pb_update(refs[[i]]$package)
    new_refs <- c(new_refs, list(get_ref_release(refs[[i]])))
  }
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
new_min_deps_installation_proposal <- function( # nolint
                                               path,
                                               config = list(
                                                 dependencies = .desc_field,
                                                 library = tempfile()
                                               )) {
  path <- normalizePath(path)

  config$dependencies <- .desc_field
  if ("library" %nin% names(config)) {
    config$library <- tempfile()
  }

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  new_refs <- list()

  cli_pb_init("min", length(refs))
  for (i in seq_along(refs)) {
    pkg <- refs[[i]]$package
    version <- subset(d$get_deps(), package == pkg, "version")[[1]]
    if (version == "*") {
      op <- op_ver <- ""
    } else {
      op <- strsplit(version, " ")[[1]][1]
      op_ver <- strsplit(version, " ")[[1]][2]
    }
    cli_pb_update(pkg)
    new_refs <- c(new_refs, list(get_ref_min_incl_cran(refs[[i]], op, op_ver)))
  }
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")

  d <- desc_cond_set_refs(d, new_refs_str)

  res <- desc_to_ip(d, config)
  class(res) <- c("min_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' Read DESCRIPTION file and return list of references.
#' Returned list is an union between references specified in `"Config/Needs/verdepcheck"` field and
#' standard references for all other not covered dependencies.
#' @importFrom pkgdepends pkg_dep_types parse_pkg_ref
#' @importFrom utils installed.packages
#' @keywords internal
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' d <- desc::desc("!new")
#' d$set_dep("foo", "Import")
#' d$set_dep("bar", "Suggest")
#' d$set_list("Config/Needs/verdepcheck", "foo/bar")
#' d$set_list("Config/Needs/verdepcheck", "foo/baz") # not in pacakge deps - will be skipped
#' get_refs_from_desc(d)
get_refs_from_desc <- function(d) {
  if (.desc_field %nin% d$fields()) {
    return(list())
  }
  all_deps <- subset(d$get_deps(), type %in% pkgdepends::pkg_dep_types(), "package")[[1]]
  refs <- lapply(
    trimws(strsplit(d$get_field(.desc_field), ",")[[1]]),
    pkgdepends::parse_pkg_ref
  )
  base_pkgs <- c("R", rownames(utils::installed.packages(priority = "base")))
  missing_refs <- setdiff(setdiff(all_deps, base_pkgs), vapply(refs, `[[`, character(1), "package"))
  res <- c(
    refs,
    lapply(missing_refs, pkgdepends::parse_pkg_ref)
  )
  res_idx <- match(all_deps, vapply(res, `[[`, character(1), "package"))
  res_idx <- res_idx[!is.na(res_idx)]
  res[res_idx]
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

#' @importFrom cli cli_progress_bar
#' @keywords internal
cli_pb_init <- function(type, total) {
  cli::cli_progress_bar(
    format = paste(
      "{cli::pb_spin} Resolving {cli::pb_extra$type} version {cli::pb_extra$package}",
      "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
    ),
    format_done = paste0(
      "{cli::col_green(cli::symbol$tick)} Resolved {cli::pb_total} packages in {cli::pb_elapsed}."
    ),
    extra = list(type = type, package = character(0)),
    total = total,
    .envir = parent.frame(2L)
  )
}
#' @importFrom cli cli_progress_update
#' @keywords internal
cli_pb_update <- function(package) {
  cli::cli_progress_update(extra = list(package = package), .envir = parent.frame(2L))
}
