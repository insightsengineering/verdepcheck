expect_latest_ppm <- function(uri) {
  expect_no_match(uri, "/[0-9]{4}-[0-9]{2}-[0-9]{2}$")
}

skip_if_offline <- function() {
  # Using this conditional clause instead of `skip_if_not_installed` to provide
  #  a better message
  if (!requireNamespace("pingr", quietly = TRUE)) {
    skip("`pingr` cannot be loaded, can't test internet connection.")
  }
  res <- tryCatch(
    pingr::ping("api.github.com", count = 1L),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}

skip_if_empty_gh_token <- function() {
  res <- tryCatch(
    gh::gh_token() != "",
    error = function(e) FALSE
  )

  if (isFALSE(res)) skip("Not run with empty GH token")
}

#' Aggregator of tests to generally perform on proposals
#'
#' @param x (`pkg_installation_proposal` object) Valid proposal created by one
#' of the available methods.
#' @param pkg_name (`string`) Name of package that is being tested for version.
#' @param platform (optional `string`) Name of the platform, should be 'source' in
#' most cases.
#' @param pkg_ver_target (optional `string`) version that is expected to be in the
#' proposal. A `NULL` value indicates to use the latest version on CRAN or a
#' GitHub repository reference
#' @param pkg_gh_str (optional `string`) GitHub repository reference to retrieve
#' the version that is on the main branch. When both this parameter and
#' `pkg_ver_target` are `NULL`, then it will compare the version in the proposal
#' with the latest version in CRAN.
#' @param solve_ip_flag (optional `logical`) indicates if the installation
#' proposal should be solved
#'
#' @keywords internal
test_proposal_common <- function(x,
                                 pkg_name = "pkgdepends",
                                 platform = "source",
                                 pkg_ver_target = NULL,
                                 pkg_gh_str = NULL,
                                 solve_ip_flag = TRUE) {
  expect_s3_class(x, "pkg_installation_proposal")

  # Allows to re-use x accross packages without having to solve it again
  if (solve_ip_flag) solve_ip(x)

  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(
    x_solution,
    package == pkg_name & platform == "source"
  )

  expect_true(nrow(x_solution_pkg) >= 1)
  expect_true(any(x_solution_pkg$status == "OK"))

  pkg_ver_act <- max(package_version(x_solution_pkg$version))

  # If there is no specific version to check, then compare against latest from
  #  CRAN
  if (is.null(pkg_gh_str) && is.null(pkg_ver_target)) {
    pkg_ver_target <- package_version(
      available.packages(
        repos = pkgcache::default_cran_mirror(),
        filters = list(
          add = TRUE, function(x) x[x[, "Package"] == pkg_name, ]
        )
      )[["Version"]]
    )
  } else if (!is.null(pkg_gh_str) && is.null(pkg_ver_target)) {
    gh_str_split <- strsplit(pkg_gh_str, "/")[[1]]
    pkg_ver_target <- package_version(as.character(
      get_desc_from_gh(gh_str_split[1], gsub("@.*$", "", gh_str_split[2]))$get_version()
    ))
  }

  expect_identical(pkg_ver_act, package_version(pkg_ver_target))

  invisible(x)
}

#' @inheritParams test_proposal_common
#' @keywords internal
test_proposal_common_bioc <- function(x,
                                      pkg_name = "pkgdepends",
                                      platform = "source",
                                      solve_ip_flag = TRUE) {
  expect_s3_class(x, "pkg_installation_proposal")

  # Allows to re-use x accross packages without having to solve it again
  if (solve_ip_flag) solve_ip(x)

  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(
    x_solution,
    package == pkg_name & platform == "source"
  )

  expect_true(nrow(x_solution_pkg) >= 1)
  expect_true(any(x_solution_pkg$status == "OK"))

  invisible(x)
}
