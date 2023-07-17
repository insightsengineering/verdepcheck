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

#' Temporarily create a valid DESCRIPTION file to a temporary location
#'
#' The file is deleted after the parent function where this function was called
#' has exited, when the R session ends or on deman via [withr::deferred_run()]
#' @param pkg_list (`vector`) named character vector or list with
#' paired name and type of dependency. It supports versions by using quotes on
#' the key
#' @param remotes (`vector`) string vector that contains remotes to add to
#' the DESCRIPTION file
#' @param need_verdepcheck (`vector`) string vector that contains
#' Config/Need/verdepcheck elements to add to the DESCRIPTION file
#' @param .local_envir (`envirnoment`) The environment to use for scoping.
#'
#' @keywords internal
local_description <- function(pkg_list = c(pkgdepends = "Import"),
                              remotes = c(),
                              need_verdepcheck = c(),
                              .local_envir = parent.frame()) {
  d_std <- desc::desc("!new")

  for (pkg in names(pkg_list)) {
    d_std$set_dep(pkg, pkg_list[[pkg]])
  }

  for (remote in remotes) {
    d_std$add_remotes(remote)
  }

  if (!is.null(need_verdepcheck) && length(need_verdepcheck) > 0) {
    d_std$set(.desc_field, paste(need_verdepcheck, collapse = ", "))
  }

  path <- tempfile(pattern = "DESCRIPTION")
  d_std$write(path)
  withr::defer(unlink(path), envir = .local_envir)

  path
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
#'
#' @keywords internal
test_proposal_common <- function(x,
                                 pkg_name = "pkgdepends",
                                 platform = "source",
                                 pkg_ver_target = NULL,
                                 pkg_gh_str = NULL) {
  expect_s3_class(x, "pkg_installation_proposal")

  solve_ip(x)

  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(
    x_solution,
    package == pkg_name & platform == "source" & repotype == "cran"
  )

  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)

  # If there is no specific version to check, then compare against latest from
  #  CRAN
  if (is.null(pkg_gh_str) && is.null(pkg_ver_target)) {
    pkg_ver_target <- package_version(
      available.packages(
        repos = pkgcache::default_cran_mirror(),
        filters = list(
          add = TRUE, function(x) x[x[, "Package"] == "pkgdepends", ]
        )
      )[["Version"]]
    )
  } else if (!is.null(pkg_gh_str) && is.null(pkg_ver_target)) {
    gh_str_split <- strsplit(pkg_gh_str, "/")[[1]]
    pkg_ver_target <- package_version(as.character(
      get_desc_from_gh(gh_str_split[1], gh_str_split[2])$get_version()
    ))
  }

  expect_identical(pkg_ver_act, package_version(pkg_ver_target))
  invisible(x)
}
