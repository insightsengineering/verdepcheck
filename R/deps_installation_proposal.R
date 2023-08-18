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

  d <- desc_cond_set_refs(d, new_refs_str)
  d <- desc_remotes_cleanup(d)

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
      version <- version_from_desc(x$package, d)
      if (inherits(x, "remote_ref_github") && check_if_on_cran(x, version) && x$commitish == "") {
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
        version <- version_from_desc(x$package, d)
        get_ref_min(x, version$op, version$op_ver)
      } else {
        x
      }
    }
  )
  new_refs_str <- vapply(new_refs, `[[`, character(1), "ref")
  d <- desc_cond_set_refs(d, new_refs_str)
  d <- desc_remotes_cleanup(d)

  # find PPM snapshot
  refs <- get_refs_from_desc(d)
  refs_pkg <- vapply(refs, `[[`, character(1), "package")
  deps <- d$get_deps()

  dependencies_config_cache <- tolower(
    pkgdepends::as_pkg_dependencies(config$dependencies)$direct
  )

  deps_release_dates <- lapply(
    seq_len(nrow(deps)),
    function(i) {
      i_pkg <- deps[i, "package"]

      if (tolower(deps[i, "type"]) %nin% dependencies_config_cache) {
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

      version <- version_from_desc(i_ref$package, d)
      i_ref_ver <- get_ref_min(i_ref, version$op, version$op_ver)

      get_release_date(i_ref_ver)
    }
  )

  # Obtain the maximum release data of all the dependecies
  max_release_date <- as.Date(
    max(
      -Inf, # Suppress warning when running max() with all NA and `na.rm = TRUE`
      unlist(
        lapply(deps_release_dates, as.Date, origin = "1970-01-01")
      ),
      na.rm = TRUE
    ),
    origin = "1970-01-01"
  )

  ppm_repo <- get_ppm_snapshot_by_date(max_release_date)

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
      version <- version_from_desc(x$package, d)
      if (
        inherits(x, "remote_ref_github") &&
          check_if_on_cran(x, version) &&
          x$commitish == ""
      ) {
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
        version <- version_from_desc(x$package, d)
        get_ref_min(x, version$op, version$op_ver)
      } else {
        x
      }
    }
  )
  new_refs_str <- map_key_character(new_refs, "ref")

  d <- desc_cond_set_refs(d, new_refs_str)
  d <- desc_remotes_cleanup(d)

  res <- desc_to_ip(d, config)
  class(res) <- c("min_isolated_deps_installation_proposal", "deps_installation_proposal", class(res))
  res
}
