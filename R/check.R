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
  tryCatch(
    ip$install(),
    error = function(err) {
      # Print compilation error when installation fails to help debug
      print(err)
      stop(err)
    }
  )

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
