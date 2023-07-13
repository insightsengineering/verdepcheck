skip_if_offline <- function() {
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

local_description <- function(pkg_list = list(pkgdepends = "Import"),
                              remotes = list(),
                              .local_envir = parent.frame()) {

  d_std <- desc::desc("!new")

  for (pkg in names(pkg_list)) {
    d_std$set_dep(pkg, pkg_list[[pkg]])
  }

  for (remote in remotes) {
    d_std$add_remotes(remote)
  }

  path <- tempfile(pattern = "DESCRIPTION")
  d_std$write(path)
  withr::defer(unlink(path), envir = .local_envir)

  path
}

test_proposal_common <- function(x,
                                 pkg_name = "pkgdepends",
                                 platform = "source",
                                 pkg_ver_target = NULL) {
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
  if (is.null(pkg_ver_target)) {
    pkg_ver_target <- package_version(
      available.packages(
        repos = pkgcache::default_cran_mirror(),
        filters = list(add = TRUE, function(x) x[x[, "Package"] == "pkgdepends", ])
      )[["Version"]]
    )
  }

  expect_identical(pkg_ver_act, package_version(pkg_ver_target))
  invisible(x)
}
