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
#'   list(`dplyr (>= 1.1.2)` = "Import", `checkmate (>= 2.1.0)` = "Import")
#' )
#' verdepcheck:::install_deps_individually_tmp(desc_path)
install_deps_individually_tmp <- function(path) {
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
      install_pkg_versions_tmp(
        package = .x$package,
        op = version$op,
        op_ver = version$op_ver,
        max_count = 1,
        throw_error = TRUE
      )
    }
  )
}

#' Test install all the versions of the package locally
#'
#' Support function to assess if every version specified by `op` and `op_ver`
#' of the package can be installed. This is not always the case as there might
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
#' @param throw_error (`logical`) flag that indicates it should stop and throw
#' an error when it cannot install a version of a package (instead of continuing)
#'
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' install_pkg_versions_tmp("checkmate", ">=", "2.1.0")
#' install_pkg_versions_tmp("Rcpp", ">=", "0.12.15", 3, FALSE)
#' install_pkg_versions_tmp("Rcpp", ">=", "0.12.15", NA, TRUE)
#'
#' # Make sure the latest version on CRAN is working
#' dplyr_latest <- pkgcache::meta_cache_list("dplyr")$version[1]
#' install_pkg_versions_tmp("dplyr", ">=", dplyr_latest)
install_pkg_versions_tmp <- function(package,
                                     op = "",
                                     op_ver = "",
                                     max_count = NA,
                                     until_success = TRUE,
                                     throw_error = FALSE) {
  # Get metadata on latest release from CRAN
  column_mapping <- c(
    package = "package", version = "version", raw = "target",
    mtime = "published", url = "sources", mirror = "sources"
  )
  pkg_latest <- pkgcache::meta_cache_list(package)[, column_mapping]
  colnames(pkg_latest) <- names(column_mapping)

  # Conversion between metadata of `meta_cache_list` and `cran_archive_list`
  pkg_latest$url <- vapply(pkg_latest$url, function(.x) .x[[1]][1], character(1))
  pkg_latest$mirror <- pkg_latest$url
  pkg_latest$raw <- file.path(package, basename(pkg_latest$raw))

  # Get all versions from CRAN, but only keep those that match op / op_version
  pkg_versions <- rbind(
    pkgcache::cran_archive_list(packages = package), # Get versions from archive
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
    install_result[[el]] <- tryCatch(
      {
        ip$solve()
        ip$download()
        ip$install()
        list(
          package = row$package,
          version = row$version,
          fun = cli::cli_alert_success,
          status = 0L)
      },
      error = function(err) {
        if (throw_error) stop(err)
        print(err)
        list(
          package = row$package,
          version = row$version,
          fun = cli::cli_alert_danger,
          status = -1L
        )
      }
    )

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

  invisible(install_result)
}
