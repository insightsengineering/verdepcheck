#' Executes installation plan.
#'
#' This function would executes the following:
#' * solves package dependencies
#' * downloads all package dependencies
#' * installs system requirements
#' * installs all package dependencies
#'
#' @param ip (`pkg_installation_plan`) object to execute
#'
#' @returns `pkg_installation_plan` object invisibly
#'
#' @keywords internal
install_ip <- function(ip) {
  try({
    solve_ignore_remotes_release(ip)
    ip$stop_for_solution_error()

    ip$download()
    ip$stop_for_download_error()

    ip$install_sysreqs()
    ip$install()
  })

  return(invisible(ip))
}

#' Solve installation plan ignoring entries with "@*release" remote refs for detected conflicts.
#' @keywords internal
solve_ignore_remotes_release <- function(ip) {
  UseMethod("solve_ignore_remotes_release", ip)
}

#' @exportS3Method solve_ignore_remotes_release pkg_deps
solve_ignore_remotes_release.pkg_deps <- function(ip) {
  ip$solve()
  return(invisible(ip))
}

#' @exportS3Method solve_ignore_remotes_release pkg_installation_proposal
solve_ignore_remotes_release.pkg_installation_proposal <- function(ip) {
  solve_ignore_remotes_release.pkg_deps(ip)
}

#' @exportS3Method solve_ignore_remotes_release deps_installation_proposal
solve_ignore_remotes_release.deps_installation_proposal <- function(ip) {
  # ugly hack! - overwrite resolution result before calling solve
  # replace "@*release" GH refs to the "@<ref for min ver>" for all direct dependent pkgs to avoid conflicts
  # use case:
  # foo -imports-> bar (>= 1.2.3) & baz (>= 1.2.3) (and has bar@*release and baz@*release in its Remotes)
  # bar -imports-> baz (and has baz@*release in its Remotes)
  # when doing min_deps we identify min version of baz to be 1.2.3
  # there is a conflict between baz@1.2.3 and baz@*release

  ip$resolve()

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

#' Executes [`rcmdcheck::rcmdcheck()`] on a local package using `libpath` from the installation plan.
#'
#' @param ip (`pkg_installation_plan`) object to extract `libpath` from
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
#' @keywords internal
check_ip <- function(ip,
                     path,
                     build_args = character(),
                     check_args = character(),
                     ...) {
  libpath <- ip$get_config()$get("library")

  res <- tryCatch(
    rcmdcheck::rcmdcheck(
      path,
      libpath = libpath,
      args = check_args,
      build_args = build_args,
      error_on = "never",
      ...
    ),
    error = function(e) {
      NULL
    }
  )

  return(res)
}

#' Executes installation plan and [`rcmdcheck::rcmdcheck()`]
#'
#' @inheritParams install_ip
#' @inheritParams check_ip
#'
#' @return a named `list` with two elements:
#' * `"ip"` - installation plan object
#' * `"check"` - returned value from [`rcmdcheck::rcmdcheck()`]
#'
#' @keywords internal
deps_check_internal <- function(ip, path, build_args, check_args, ...) {
  ip <- install_ip(ip)
  check_res <- check_ip(ip, path, build_args, check_args, ...)

  return(invisible(list(ip = ip, check = check_res)))
}

#' Execute `R CMD CHECK` on a local package with all dependencies pre-installed using various strategies.
#'
#' @inheritSection new_max_deps_installation_proposal strategies
#' @inherit new_max_deps_installation_proposal note
#'
#' @inheritParams new_max_deps_installation_proposal
#' @inheritParams check_ip
#'
#' @inherit deps_check_internal return
#'
#' @seealso [deps_installation_proposal]
#'
#' @rdname deps_check
#'
#' @export
max_deps_check <- function(path,
                           config = list(),
                           build_args = character(),
                           check_args = character(),
                           ...) {
  ip <- new_max_deps_installation_proposal(path, config)
  deps_check_internal(ip, path, check_args, build_args, ...)
}

#' @rdname deps_check
#' @export
release_deps_check <- function(path,
                               config = list(),
                               check_args = character(),
                               build_args = character(),
                               ...) {
  ip <- new_release_deps_installation_proposal(path, config)
  deps_check_internal(ip, path, check_args, build_args, ...)
}

#' @rdname deps_check
#' @export
min_deps_check <- function(path,
                           config = list(),
                           check_args = character(),
                           build_args = character(),
                           ...) {
  ip <- new_min_deps_installation_proposal(path, config)
  deps_check_internal(ip, path, check_args, build_args, ...)
}
