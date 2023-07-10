#' Execute `R CMD CHECK` on a local package with all dependencies pre-installed using various strategies.
#'
#' @inheritSection new_max_deps_installation_proposal strategies
#' @inherit new_max_deps_installation_proposal note
#'
#' @inheritParams new_max_deps_installation_proposal
#' @inheritParams execute_ip
#'
#' @inherit execute_ip return
#'
#' @seealso [deps_installation_proposal]
#'
#' @rdname deps_check
#'
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- max_deps_check(".")
#' x$ip
#' x$check
max_deps_check <- function(path,
                           config = list(),
                           build_args = character(),
                           check_args = character(),
                           ...) {
  ip <- new_max_deps_installation_proposal(path, config)
  execute_ip(ip, path, check_args, build_args, ...)
}

#' @rdname deps_check
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- release_deps_check(".")
#' x$ip
#' x$check
release_deps_check <- function(path,
                               config = list(),
                               build_args = character(),
                               check_args = character(),
                               ...) {
  ip <- new_release_deps_installation_proposal(path, config)
  execute_ip(ip, path, check_args, build_args, ...)
}

#' @rdname deps_check
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- min_cohort_deps_check(".")
#' x$ip
#' x$check
min_cohort_deps_check <- function(path,
                                  config = list(),
                                  build_args = character(),
                                  check_args = character(),
                                  ...) {
  ip <- new_min_cohort_deps_installation_proposal(path, config)
  execute_ip(ip, path, check_args, build_args, ...)
}

#' @rdname deps_check
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' x <- min_isolated_deps_check(".")
#' x$ip
#' x$check
min_isolated_deps_check <- function(path,
                                    config = list(),
                                    build_args = character(),
                                    check_args = character(),
                                    ...) {
  ip <- new_min_isolated_deps_installation_proposal(path, config)
  execute_ip(ip, path, check_args, build_args, ...)
}

#' Executes installation plan and [`rcmdcheck::rcmdcheck()`] in "try mode" to always return.
#'
#' @param ip (`pkg_installation_plan`) object to execute
#' @inheritParams check_ip
#'
#' @return a named `list` with two elements:
#' * `"ip"` - installation plan object
#' * `"check"` - returned value from [`rcmdcheck::rcmdcheck()`]
#'
#' @export
execute_ip <- function(ip, path, build_args, check_args, ...) {
  check_res <- NULL
  try({
    ip <- solve_ip(ip)
    ip <- download_ip(ip)
    ip <- install_ip(ip)
    check_res <- check_ip(ip, path, build_args, check_args, ...)
  })

  return(invisible(list(ip = ip, check = check_res)))
}


#' Try to solve using standard method. If error - use [resolve_ignoring_release_remote].
#'
#' @inheritParams check_ip
#'
#' @returns `pkg_installation_plan` object invisibly
#'
#' @export
solve_ip <- function(ip) {
  UseMethod("solve_ip", ip)
}
#' @exportS3Method solve_ip deps_installation_proposal
solve_ip.deps_installation_proposal <- function(ip) {
  ip$solve()
  resolve_ignoring_release_remote(ip)
}

#' For each direct dependency, resolve that package using PPM snapshot as of release date + 1.
#' Finally, combine resolutions and run solve.
#' @keywords internal
#' @exportS3Method solve_ip min_isolated_deps_installation_proposal
solve_ip.min_isolated_deps_installation_proposal <- function(ip) { # nolint
  ip$resolve()
  res <- ip$get_resolution()
  res$parent <- NA_character_

  deps <- res[1, "deps"][[1]]
  ## copy op and version to Config\Needs\verdepcheck rows
  deps <- split(deps, as.factor(deps$package))
  deps <- lapply(deps, function(x) {
    x$op <- x$op[1]
    x$version <- x$version[1]
    x
  })
  deps <- do.call(rbind, deps)
  deps <- deps[tolower(deps$type) %in% tolower(res[1, "dep_types"][[1]]), ]

  cli_pb_init("min_isolated", total = nrow(deps))

  deps_res <- lapply(seq_len(nrow(deps)), function(i) {
    i_pkg <- deps[i, "package"]

    cli_pb_update(package = i_pkg, n = 4L)

    if (i_pkg %in% base_pkgs()) {
      return(NULL)
    }

    result <- resolve_ppm_snapshot(
      deps[i, "ref"],
      deps[i, "op"],
      deps[i, "version"]
    )
    # remove duplicate entries of current package
    result[result$ref != deps[i, "ref"] | !duplicated(result$ref),]
  })

  new_res <- do.call(rbind, deps_res)
  # Order by package name, version number and mirror
  #  for reproducible resolution
  order_index <- order( new_res$package,
    as.numeric_version(new_res$version),
    new_res$mirror
  )
  new_res <- new_res[order_index,]
  new_res <- new_res[!duplicated(new_res[,c("ref", "package", "version")]), ]

  # Keep res at top
  new_res <- rbind(res[1, ], new_res)

  # Enforce minimum Rcpp version for R > 4.0
  new_res <- enforce_rcpp(new_res)

  ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result <- new_res
  ip$solve()

  resolve_ignoring_release_remote(ip)

  return(invisible(ip))
}

#' If solution errors finishes with "dependency conflict" error then
#' re-try again ignoring "@*release" remote refs for detected conflicts.
#'
#' @inheritParams check_ip
#'
#' @inherit solve_ip return
#'
#' @keywords internal
resolve_ignoring_release_remote <- function(ip) { # nolint
  tryCatch(
    ip$stop_for_solution_error(),
    error = function(e) {
      if (!grepl("*.dependency conflict$", e$message)) stop(e)
      cat("Solve using alternative method ignoring `@*release` for conflicting refs.\n")
      solve_ip_ignore_remotes_release(ip)
      ip$stop_for_solution_error()
    }
  )
  return(invisible(ip))
}

#' Solve installation plan ignoring entries with "@*release" remote refs for detected conflicts.
#'
#' @inheritParams check_ip
#'
#' @inherit solve_ip return
#'
#' @keywords internal
solve_ip_ignore_remotes_release <- function(ip) { # nolint
  # replace "@*release" GH refs to the "@<ref for min ver>" for all direct dependent pkgs to avoid conflicts
  # use case:
  # foo -imports-> bar (>= 1.2.3) & baz (>= 1.2.3) (and has bar@*release and baz@*release in its Remotes)
  # bar -imports-> baz (and has baz@*release in its Remotes)
  # when doing min_deps we identify min version of baz to be 1.2.3
  # there is a conflict between baz@1.2.3 and baz@*release

  if (is.null(ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result)) ip$resolve()

  conflicting_pkgs <- resolution <- ip$get_resolution()

  conflicting_pkgs <- split(resolution, as.factor(conflicting_pkgs$package))
  conflicting_pkgs <- Filter(function(x) any(grepl("\\@\\*release", x$ref)), conflicting_pkgs)
  conflicting_pkgs <- Filter(function(x) length(unique(x$ref)) > 1, conflicting_pkgs)

  conflicting_pkgs_refs <- lapply(
    conflicting_pkgs,
    function(x) {
      c(
        package = x$package[1],
        old_ref = grep("\\@\\*release", x$ref, value = TRUE)[1],
        new_ref = grep("\\@\\*release", x$ref, value = TRUE, invert = TRUE)[1]
      )
    }
  )
  conflicting_pkgs_refs <- data.frame(do.call(rbind, conflicting_pkgs_refs), row.names = NULL)

  replace_using_df <- function(x, df) {
    for (i in seq_len(nrow(df))) {
      x <- replace(x, x == df[i, 1], df[i, 2])
    }
    x
  }
  for (i in seq_len(nrow(resolution))) {
    i_deps <- resolution[i, "deps"][[1]]
    if (any(i_deps$package %in% conflicting_pkgs_refs$package)) {
      i_deps$ref <- replace_using_df(i_deps$ref, conflicting_pkgs_refs[, c("old_ref", "new_ref")])
    }
    resolution[i, "deps"] <- list(list(i_deps))
  }

  ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result <- resolution

  ip$solve()

  return(invisible(ip))
}


#' Solve installation plan ignoring entries with "@*release" remote refs for detected conflicts.
#'
#' @inheritParams check_ip
#'
#' @inherit solve_ip return
#'
#' @export
download_ip <- function(ip) {
  ip$download()
  ip$stop_for_download_error()

  return(invisible(ip))
}

#' Executes installation plan.
#'
#' This function would executes the following:
#' * solves package dependencies
#' * downloads all package dependencies
#' * installs system requirements
#' * installs all package dependencies
#'
#' @inheritParams execute_ip
#'
#' @returns `pkg_installation_plan` object invisibly
#'
#' @export
install_ip <- function(ip) {
  ip$install_sysreqs()
  ip$install()

  return(invisible(ip))
}

#' Executes [`rcmdcheck::rcmdcheck()`] on a local package using `libpath` from the installation plan.
#'
#' @inheritParams execute_ip
#' @param path (`string`) path to the package sources
#' @param build_args (`string`) value passed as `build_args` argument into [`rcmdcheck::rcmdcheck()`]
#' @param check_args (`string`) value passed as `args` argument into [`rcmdcheck::rcmdcheck()`]
#' @param ... other arguments passed to [`rcmdcheck::rcmdcheck()`]
#'
#' @inherit rcmdcheck::rcmdcheck return
#'
#' @seealso [rcmdcheck::rcmdcheck()] for other configuration possibilities
#'
#' @importFrom rcmdcheck rcmdcheck
#'
#' @export
check_ip <- function(ip,
                     path,
                     build_args = character(),
                     check_args = character(),
                     ...) {
  libpath <- ip$get_config()$get("library")

  rcmdcheck::rcmdcheck(
    path,
    libpath = libpath,
    args = check_args,
    build_args = build_args,
    error_on = "never",
    ...
  )
}
