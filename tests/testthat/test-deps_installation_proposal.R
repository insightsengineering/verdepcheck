test_that("new_max_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source")
})

test_that("new_release_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_release_deps_installation_proposal(d_std_path)

  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source")
})

test_that("new_min_isolated_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.1.0")
})

test_that("new_min_isolated_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0")
})

test_that("new_min_cohort_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.1.0")
})

####

d_gh <- desc::desc("!new")
d_gh$set_dep("pkgdepends", "Import")
d_gh$add_remotes("r-lib/pkgdepends")
ref_gh_path <- tempfile()
d_gh$write(ref_gh_path)
on.exit(unlink(ref_gh_path), add = TRUE, after = FALSE)

test_that("new_max_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  x <- new_max_deps_installation_proposal(ref_gh_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(x_solution, package == "pkgdepends" & platform == "source")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)
  pkg_ver_target <- package_version(as.character(get_desc_from_gh("r-lib", "pkgdepends")$get_version()))
  expect_equal(pkg_ver_act, pkg_ver_target)
})

test_that("new_release_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  x <- new_release_deps_installation_proposal(ref_gh_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(x_solution, package == "pkgdepends" & platform == "source")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)
  pkg_ver_target <- package_version(
    available.packages(
      repos = pkgcache::default_cran_mirror(),
      filters = list(add = TRUE, function(x) x[x[, "Package"] == "pkgdepends", ])
    )[["Version"]]
  )
  expect_equal(pkg_ver_act, pkg_ver_target)
})

test_that("new_min_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  x <- new_min_deps_installation_proposal(ref_gh_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(x_solution, package == "pkgdepends" & platform == "source")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)
  pkg_ver_target <- package_version("0.1.0")

  expect_identical(pkg_ver_act, pkg_ver_target)
})

####

test_that("new_min_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  temp_path <- tempfile()
  d <- desc::desc("!new")
  d$set_dep("pkgdepends", "Import", ">= 0.2.0")
  d$add_remotes("r-lib/pkgdepends")
  d$write(temp_path)
  on.exit(unlink(temp_path), add = TRUE, after = FALSE)

  x <- new_min_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(x_solution, package == "pkgdepends" & platform == "source")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)
  pkg_ver_target <- package_version("0.2.0")
  expect_identical(pkg_ver_act, pkg_ver_target)
})

test_that("new_min_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  temp_path <- tempfile()
  d <- desc::desc("!new")
  d$set_dep("pkgdepends", "Import", ">= 0.2.0")
  d$write(temp_path)
  on.exit(unlink(temp_path), add = TRUE, after = FALSE)

  x <- new_min_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_resolution()

  x_solution_pkg <- subset(x_solution, package == "pkgdepends" & platform == "source")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- package_version(x_solution_pkg$version)
  pkg_ver_target <- package_version("0.2.0")
  expect_identical(pkg_ver_act, pkg_ver_target)
})
