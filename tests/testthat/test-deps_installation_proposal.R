d <- desc::desc("!new")
d$set_dep("pkgdepends", "Import")
d$add_remotes("r-lib/pkgdepends")
temp_path <- tempfile()
d$write(temp_path)
on.exit(unlink(temp_path), add = TRUE, after = FALSE)

test_that("new_max_deps_installation_proposal works", {
  x <- new_max_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  expect_true("pkgdepends" %in% x$get_solution()$data$package)
})

test_that("new_release_deps_installation_proposal works", {
  x <- new_release_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  expect_true("pkgdepends" %in% x$get_solution()$data$package)
})

test_that("new_min_deps_installation_proposal works", {
  x <- new_min_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  expect_true("pkgdepends" %in% x$get_solution()$data$package)
})


test_that("new_max_deps_installation_proposal correctly handles <org>/<repo> reference", {
  temp_path <- tempfile()
  d <- desc::desc("!new")
  d$set_dep("pkgdepends", "Import")
  d$add_remotes("r-lib/pkgdepends")
  d$write(temp_path)
  on.exit(unlink(temp_path), add = TRUE, after = FALSE)

  x <- new_max_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_solution()$data

  x_solution_pkg <- subset(x_solution, package == "pkgdepends")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- x_solution_pkg$version
  pkg_ver_target <- as.character(get_ver_from_gh("r-lib", "pkgdepends"))
  expect_equal(pkg_ver_act, pkg_ver_target)
})

test_that("new_release_deps_installation_proposal correctly handles <org>/<repo> reference", {
  temp_path <- tempfile()
  d <- desc::desc("!new")
  d$set_dep("pkgdepends", "Import")
  d$add_remotes("r-lib/pkgdepends")
  d$write(temp_path)
  on.exit(unlink(temp_path), add = TRUE, after = FALSE)

  x <- new_release_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_solution()$data

  x_solution_pkg <- subset(x_solution, package == "pkgdepends")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- x_solution_pkg$version
  pkg_ver_target <- as.character(get_ver_from_gh("r-lib", "pkgdepends"))
  expect_match(pkg_ver_act, "^[0-999].[0-999].[0-999]$") # @TODO
})

test_that("new_min_deps_installation_proposal correctly handles <org>/<repo> reference", {
  temp_path <- tempfile()
  d <- desc::desc("!new")
  d$set_dep("pkgdepends", "Import")
  d$add_remotes("r-lib/pkgdepends")
  d$write(temp_path)
  on.exit(unlink(temp_path), add = TRUE, after = FALSE)

  x <- new_min_deps_installation_proposal(temp_path)
  on.exit(unlink(x$get_config()$library), add = TRUE, after = FALSE)

  expect_s3_class(x, "pkg_installation_proposal")

  x$solve()
  expect_equal(x$get_solution()$status, "OK")

  x_solution <- x$get_solution()$data

  x_solution_pkg <- subset(x_solution, package == "pkgdepends")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- x_solution_pkg$version
  pkg_ver_target <- "0.1.0"
  expect_equal(pkg_ver_act, pkg_ver_target)
})

test_that("new_min_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> reference", {
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

  x_solution <- x$get_solution()$data

  x_solution_pkg <- subset(x_solution, package == "pkgdepends")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- x_solution_pkg$version
  pkg_ver_target <- "0.2.0"
  expect_equal(pkg_ver_act, pkg_ver_target)
})

test_that("new_min_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
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

  x_solution <- x$get_solution()$data

  x_solution_pkg <- subset(x_solution, package == "pkgdepends")
  expect_equal(nrow(x_solution_pkg), 1)

  pkg_ver_act <- x_solution_pkg$version
  pkg_ver_target <- "0.2.0"
  expect_equal(pkg_ver_act, pkg_ver_target)
})
