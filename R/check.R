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
  on.exit(return(invisible(ip)), add = TRUE)

  ip$solve()
  ip$stop_for_solution_error()

  ip$download()
  ip$stop_for_download_error()

  ip$install_sysreqs()
  ip$install()

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
#' @seealso `rcmdcheck::rcmdcheck()` for other configuration possibilities
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

  rcmdcheck::rcmdcheck(
    path,
    libpath = libpath,
    args = check_args,
    build_args = build_args,
    ...
  )
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
#' @seealso deps_installation_proposal
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
