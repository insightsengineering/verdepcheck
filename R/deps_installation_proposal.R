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
#' * `max` - use the greatest version of dependent packages. Please note that using development version is not
#' guaranteed to be stable.
#' See [get_ref_max] for details.
#' * `release` - use the released version of dependent packages. It will try use CRAN if possible else if
#' GitHub release is available then use it else fail.
#' See [get_ref_release] for details.
#' * `min_cohort` - find maximum date of directly dependent packages release dates and use that as PPM snapshot date
#' for dependency resolve.
#' * `min_isolated` - for each direct dependency: find its release date and use it as PPM snapshot for resolving itself.
#' Next, combine all the individual resolutions and resolve it altogether again.
#'
#' Both "min" strategies relies on PPM snapshot in order to limit the versions of indirect dependencies so that
#' dependency resolution ends with a package released no earlier than any of its dependency.
#' However, that's not always true for `min_isolated` strategy - done on purpose.
#'
#' Please note that only `min_cohort` and `min_isolated` strategies are "stable". The rest are basing on dynamic
#' references therefore it results might be different without changes in tested package.
#' The most straightforward example is `max` strategy in which the environment will be different after any push of
#' any of the dependencies.
#'
#' @section configuration:
#' `verdepcheck` will look into `"Config/Needs/verdepcheck"` field of the `DESCRIPTION` file for dependent packages
#' references. See [`pkgdepends::pkg_refs`] for details and this package `DESCRIPTION` file for an example.
#' Please note that some features are enabled only for package references from GitHub.
#' If you specify additional details (i.e. tag, commit, PR or `@*release`) in the reference then it wouldn't be changed.
#' Therefore, in order to make full use of various strategies, it is recommended to specify general reference in form of
#' `[<package>=][github::]<username>/<repository>[/<subdir>]` - i.e. without `[<detail>]` part.
#' Please see also [`pkgdepends::pkg_config`] and [`pak::pak-config`] for other configuration possibilities.
#'
#' @param path (`string`) path to the package sources
#' @param config (`list`) configuration options. See [`pkgdepends::pkg_config`] for details.
#' `"dependencies"` and `"library"` elements are overwritten by package level defaults.
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
new_max_deps_installation_proposal <- function(path, # nolint
                                               config = list()) {
  path <- normalizePath(path)
  config <- append_config(default_config(), config)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  new_refs <- list()

  cli_pb_init("max", length(refs))
  for (i in seq_along(refs)) {
    cli_pb_update(refs[[i]]$package)
    new_refs <- c(new_refs, list(get_ref_max(refs[[i]])))
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
new_release_deps_installation_proposal <- function(path, # nolint
                                                   config = list()) {
  path <- normalizePath(path)
  config <- append_config(default_config(), config)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  new_refs <- list()

  cli_pb_init("release", length(refs))
  for (i in seq_along(refs)) {
    cli_pb_update(refs[[i]]$package)
    new_refs <- c(new_refs, list(get_ref_release(refs[[i]])))
  }
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")

  d <- desc_remotes_cleanup(d, new_refs)
  d <- desc_cond_set_refs(d, new_refs_str)

  res <- desc_to_ip(d, config)
  class(res) <- c("release_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends as_pkg_dependencies parse_pkg_ref
#' @importFrom pkgcache ppm_repo_url
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_min_cohort_deps_installation_proposal(".")
#' solve_ip(x)
#' x$get_solution()
new_min_cohort_deps_installation_proposal <- function(path, # nolint
                                                      config = list()) {
  path <- normalizePath(path)
  config <- append_config(default_config(), config)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  # convert github to standard if possible
  new_refs <- lapply(
    refs,
    function(x) {
      if (inherits(x, "remote_ref_github") && check_if_on_cran(x) && x$commitish == "") {
        pkgdepends::parse_pkg_ref(x$package)
      } else {
        x
      }
    }
  )
  # for github type - find ref for min version and add it to the GH ref
  new_refs <- lapply(
    new_refs,
    function(x) {
      if (inherits(x, "remote_ref_github")) {
        version <- subset(d$get_deps(), package == x$package, version)[[1]]
        if (version == "*") {
          get_ref_min(x)
        } else {
          op <- strsplit(version, " ")[[1]][1]
          op_ver <- strsplit(version, " ")[[1]][2]
          get_ref_min(x, op, op_ver)
        }
      } else {
        x
      }
    }
  )
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")
  d <- desc_remotes_cleanup(d, new_refs)
  d <- desc_cond_set_refs(d, new_refs_str)

  # find PPM snapshot
  refs <- get_refs_from_desc(d)
  refs_pkg <- vapply(refs, `[[`, character(1), "package")
  deps <- d$get_deps()
  deps_release_dates <- lapply(
    seq_len(nrow(deps)),
    function(i) {
      i_pkg <- deps[i, "package"]

      if (tolower(deps[i, "type"]) %nin% tolower(pkgdepends::as_pkg_dependencies(config$dependencies)$direct)) {
        return(NA)
      }
      if (i_pkg %in% base_pkgs()) {
        return(NA)
      }
      if (i_pkg %nin% refs_pkg) {
        return(NA)
      }

      i_ref <- refs[[which(refs_pkg == i_pkg)]]

      i_ver <- deps[i, "version"]
      if (i_ver == "*") {
        i_ref_ver <- get_ref_min(i_ref)
      } else {
        i_op <- strsplit(i_ver, " ")[[1]][1]
        i_op_ver <- strsplit(i_ver, " ")[[1]][2]
        i_ref_ver <- get_ref_min(i_ref, i_op, i_op_ver)
      }

      get_release_date(i_ref_ver)
    }
  )
  max_release_date <- as.character(
    as.Date(
      max(
        unlist(
          lapply(deps_release_dates, as.Date, origin = "1970-01-01")
        ),
        na.rm = TRUE
      ),
      origin = "1970-01-01"
    )
  )
  if (is.na(max_release_date)) {
    ppm_repo <- file.path(pkgcache::ppm_repo_url(), "latest")
  } else {
    ppm_repo <- parse_ppm_url(get_ppm_snapshot_by_date(max_release_date))
  }
  config <- append_config(config, list("cran_mirror" = ppm_repo))

  res <- desc_to_ip(d, config)
  class(res) <- c("min_cohort_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends parse_pkg_ref
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- new_min_isolated_deps_installation_proposal(".")
#' solve_ip(x)
#' x$get_solution()
new_min_isolated_deps_installation_proposal <- function(path, # nolint
                                                        config = list()) {
  path <- normalizePath(path)
  config <- append_config(default_config(), config)

  d <- desc::desc(path)

  refs <- get_refs_from_desc(d)
  # convert github to standard if possible
  new_refs <- lapply(
    refs,
    function(x) {
      if (inherits(x, "remote_ref_github") && check_if_on_cran(x) && x$commitish == "") {
        pkgdepends::parse_pkg_ref(x$package)
      } else {
        x
      }
    }
  )
  # for github type - find ref for min version and add it to the GH ref
  new_refs <- lapply(
    new_refs,
    function(x) {
      if (inherits(x, "remote_ref_github")) {
        version <- subset(d$get_deps(), package == x$package, version)[[1]]
        if (version == "*") {
          get_ref_min(x)
        } else {
          op <- strsplit(version, " ")[[1]][1]
          op_ver <- strsplit(version, " ")[[1]][2]
          get_ref_min(x, op, op_ver)
        }
      } else {
        x
      }
    }
  )
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")

  d <- desc_remotes_cleanup(d, new_refs)
  d <- desc_cond_set_refs(d, new_refs_str)

  res <- desc_to_ip(d, config)
  class(res) <- c("min_isolated_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}

#' Read DESCRIPTION file and return list of references.
#' Returned list is an union between references specified in `"Config/Needs/verdepcheck"` field and
#' standard references for all other not covered dependencies.
#' @importFrom pkgdepends pkg_dep_types parse_pkg_ref
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
    refs <- list()
  } else {
    refs <- lapply(
      trimws(strsplit(d$get_field(.desc_field), ",")[[1]]),
      pkgdepends::parse_pkg_ref
    )
  }
  all_deps <- subset(d$get_deps(), type %in% pkgdepends::pkg_dep_types(), "package")[[1]]
  missing_refs <- setdiff(setdiff(all_deps, base_pkgs()), vapply(refs, `[[`, character(1), "package"))
  res <- c(
    refs,
    lapply(missing_refs, pkgdepends::parse_pkg_ref)
  )
  res_idx <- match(all_deps, vapply(res, `[[`, character(1), "package"))
  res_idx <- res_idx[!is.na(res_idx)]
  res[res_idx]
}

#' Replace Remotes in the `desc` that have been resolved to a GitHub tag or are
#' in CRAN
#'
#' Replaces any existing Remotes entry with the resolved GitHub tag from the
#' `new_refs`.
#'
#' It keeps all the existing Remotes that have not been resolved in `new_refs`.
#'
#' @param d (`desc`) DESCRIPTION object
#' @param new_refs (`list`) remote references that have been resolved and are
#' being updated in `Config/Needs/verdepcheck`
#' @keywords internal
desc_remotes_cleanup <- function(d, new_refs) {
  # Parse the remotes to retrieve the package names
  remotes <- pkgdepends::parse_pkg_refs(d$get_remotes())

  # Get the packages defined in remotes
  #  (making sure that only packages that are already defined here are modified)
  remotes_pkg <- vapply(remotes, `[[`, character(1), "package")

  # Find which packages of the new_refs are defined in Remotes
  new_refs_remotes <- Filter(
    function(.x) {
      isTRUE(.x$package %in% remotes_pkg) && inherits(.x, "remote_ref_github")
    },
    new_refs
  )

  # New remotes ref to use when replacing
  new_ref_remote <- vapply(new_refs_remotes, `[[`, character(1), "ref")

  new_ref_pkg <- vapply(new_refs, `[[`, character(1), "package")

  # Remove from `Remotes` all package that have been resolved to
  #  * CRAN package
  #  * GitHub tag
  new_remotes <- c(
    # Keep remotes (if the DESCRIPTION file is correct, this should have no elements)
    d$get_remotes()[!(remotes_pkg %in% new_ref_pkg)],
    # Modified remotes
    new_ref_remote
  )

  # Remotes that are not in new_refs are kept, as well as the ones that were
  #  resolved to be a github repo
  d$clear_remotes()

  # Return clause without Remotes section
  if (is.null(new_remotes) || length(new_remotes) == 0) return(d)
  d$set_remotes(new_remotes)
  d
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

#' Create `cli` progress bar for resolving versions.
#' @importFrom cli cli_progress_bar col_green pb_current pb_elapsed pb_eta pb_extra pb_spin pb_total symbol
#' @keywords internal
cli_pb_init <- function(type, total, ...) {
  cli::cli_progress_bar(
    format = paste(
      "{cli::pb_spin} Resolving",
      "{cli::style_bold(cli::col_yellow(cli::pb_extra$type))}",
      "version of {cli::col_blue(cli::pb_extra$package)}",
      "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
    ),
    format_done = paste0(
      "{cli::col_green(cli::symbol$tick)} Resolved {cli::pb_total} packages in {cli::pb_elapsed}."
    ),
    extra = list(type = type, package = character(0)),
    total = total,
    .envir = parent.frame(2L),
    ...
  )
}
#' @importFrom cli cli_progress_update
#' @keywords internal
cli_pb_update <- function(package, n = 2L, ...) {
  cli::cli_progress_update(extra = list(package = package), .envir = parent.frame(n), ...)
}
