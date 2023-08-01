#' Test the install of all possible versions of the dependencies in isolation.
#'
#' It looks at all dependencies individually and tries to install all possible
#' versions that exist on CRAN. This is a support function to help assess the
#' best minimal version of a package that builds in the environment.
#'
#' @details
#' Please note that because a given version of a package can be installed, it
#' does not mean that the API is compatible nor that it will be this version
#' that is selected.
#'
#' Please refer to the documentation on the existing strategies
#' for more information (e.g. [new_max_deps_installation_proposal])
#'
#' There is no output for this function other than the messages in the console.
#'
#' @param path (`character`) File path to a valid package directory that
#' contains a DESCRIPTION file.
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' desc_path <- verdepcheck:::local_description(
#'   list(`dplyr (>= 1.1.2)` = "Import")
#' )
#' verdepcheck:::test_install_deps_individually(desc_path)
test_install_deps_individually <- function(path) { # nolint
  path <- normalizePath(path)

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
      } else if (inherits(x, "remote_ref_standard")) {
        x
      } else {
        # Discard all non-CRAN packages
        NULL
      }
    }
  )

  lapply(
    new_refs,
    function(.x) {
      version <- version_from_desc(.x$package, d)
      test_install_pkg_versions(
        .x$package,
        version$op,
        version$op_ver,
        1,
        TRUE
      )
    }
  )

  invisible(NULL)
}

#' Test install all the versions of the package locally
#'
#' Support function to assess if every version specified by `op` and `op_ver`
#' of the package are installable. This is not always the case as there might
#' be some API changes that break specific versions.
#'
#' The `Rcpp` package before 0.12.16 cannot be compiled by R versions above 4.0
#'
#' There is no output for this function other than the messages in the console.
#'
#' @param package (`character`) name of R package.
#' @param op (`character`) relational operator.
#' @param op_ver (`character`) version of package.
#' @param max_count (`integer`) maximum number of versions to test, `NA` for no
#' limit.
#' @param until_success (`logical`) flag that indicates whether a
#' successful installation of a package should stop the search.
#'
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' test_install_pkg_versions("checkmate", ">=", "2.1.0")
#' test_install_pkg_versions("Rcpp", ">=", "0.12.15", 2)
#' test_install_pkg_versions("Rcpp", ">=", "0.12.15", NA, TRUE)
test_install_pkg_versions <- function(package,
                                      op = "",
                                      op_ver = "",
                                      max_count = NA,
                                      until_success = TRUE) {

  # Get all versions from CRAN, but only keep those that match op / op_version
  pkg_latest <- pkgcache::meta_cache_list(package)[, c("package", "version", "target", "published", "sources")]
  pkg_latest$sources <- vapply(pkg_latest$sources, function(.x) { .x[[1]][1] }, character(1))
  pkg_latest$mirror <- pkg_latest$sources
  colnames(pkg_latest) <- c("package", "version", "raw", "mtime", "url", "mirror")
  pkg_latest$raw <- file.path(package, basename(pkg_latest$raw))

  pkg_versions <- rbind(
    pkgcache::cran_archive_list(packages = package),
    pkg_latest
  )
  ix_versions <- check_valid_version(pkg_versions$version, op, op_ver)

  # Convert from logical array to position index capping the size on `max_count`
  max_count <- min(max_count, sum(ix_versions), na.rm = TRUE)
  ix_versions <- which(ix_versions)[seq(1, max_count)]

  # Colored blue
  package_blue_cli <- cli::col_blue(package)

  if (length(ix_versions) == 0) {
    cli::cli_alert_info("No valid versions of {{{package_blue_cli}}} found in CRAN/PPM.")
    return(invisible(NULL))
  }

  cli::cli_h1(paste0(
    "Testing the installation of {max_count} different",
    " versions of {{{package_blue_cli}}} (out of {NROW(pkg_versions)})"
  ))

  install_result <- rep(list(NULL), NROW(ix_versions))
  # Iterate on all valid versions and try to install it with ppm snapshot as
  #  CRAN mirror
  for (el in seq_along(ix_versions)) {
    row <- pkg_versions[ix_versions[el], ]
    ppm_repo <- parse_ppm_url(get_ppm_snapshot_by_date(as.Date(row$mtime) + 1))

    # Color resets to white as there might be some issues with error
    #  messages spreading it's color
    cli::cli_h2(paste0(
      "{cli::col_white(\"Attempting to install\")} {{{package_blue_cli}}} {row$version}",
      " from {cli::col_yellow(as.Date(row$mtime))}"
    ))

    # Remove directory after function exits
    tmp_lib <- withr::local_tempdir(pattern = "library-")
    new_config <- list(
      dependencies = "hard",
      cran_mirror = ppm_repo,
      library = tmp_lib
    )
    ip <- pkgdepends::new_pkg_installation_proposal(
      sprintf("%s@%s", row$package, row$version),
      config = new_config
    )

    # Try to install
    install_result[[el]] <- tryCatch({
      ip$solve()
      ip$download()
      ip$install()
      list(version = row$version, fun = cli::cli_alert_success, status = 0L)
    }, error = function(err) {
      print(err)
      list(version = row$version, fun = cli::cli_alert_danger, status = -1L)
    })

    # Break loop on first sucessful install
    if (until_success && install_result[[el]]$status == 0L) break
  }

  cli::cli_h1("Results on installing {package} versions")

  # Keep only the finished iterations
  install_result <- install_result[
    vapply(install_result, Negate(is.null), logical(1))
  ]
  for (el in install_result) {
    el$fun("{package_blue_cli} {el$version}")
  }

  if (until_success && NROW(ix_versions) != NROW(install_result)) {
    cli::cat_line()
    cli::cli_text(
      "\nNot all versions were tested as there was a succesful download/build/install"
    )
  }
}
