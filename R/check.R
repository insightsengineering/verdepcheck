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
  res <- ip$get_resolution()

  # Prevent downloads of non-binary files by removing source that has "Archive" in the URL
  # Track issue: https://github.com/r-lib/pkgdepends/issues/367
  ix <- res$platform == pkgdepends::current_r_platform()
  new_sources <- lapply(
    res$sources[ix],
    function(x) {
      if (length(x) > 1 && any(grepl("src/contrib/Archive", x))) {
        x[!grepl("src/contrib/Archive", x)]
      } else {
        x
      }
    }
  )
  ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result$sources[ix] <- new_sources

  ip$download()
  ip$stop_for_download_error()

  # Safety fallback that will try to download the binary again.
  #
  # note: that binary must be downloaded with relevant user agent with supported
  # platform and R version.
  for (ix_el in which(ix)) {
    withr::with_tempdir(
      code = {
        tar_file <- file.path(ip$get_config()$get("cache_dir"), res$target[ix_el])

        # Only do this for files that actually exist (Recommended are not )
        if (!file.exists(tar_file)) next
        untar(
          tarfile = tar_file,
          exdir = "./"
        )
        tryCatch({
          pkgdepends:::verify_extracted_package(res$package[ix_el], "./")
        }, error = function(error) {
          cli::cli_warn("{res$package[ix_el]} binary is not valid, trying to re-download.")
          # Attempts to download again using pkgcache/pkgdepends methods
          async_fun <- asNamespace("pkgcache")$async(function() {
            asNamespace("pkgcache")$download_file(
              url = file.path(res$mirror[ix_el], "src/contrib", basename(tar_file)),
              destfile = tar_file
            )
          })
          asNamespace("pkgcache")$synchronise(async_fun())
        })
      }
    )
  }

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
