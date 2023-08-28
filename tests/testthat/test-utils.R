test_that("local_description will create a valid DESCRIPTION file", {
  file_path <- local_description(
    pkg_list = list(pkgdepends = "Imports", `dplyr (>= 1.0)` = "Imports")
  )
  expect_true(file.exists(file_path))
  d <- expect_silent(
    desc::desc(file = file_path)
  )

  deps <- d$get_deps()

  expect_true("pkgdepends" %in% deps$package)
  expect_true("dplyr" %in% deps$package)

  expect_equal(deps$version[deps$package == "dplyr"], ">= 1.0")
})

test_that("local_description will create a valid DESCRIPTION file with Remotes", {
  file_path <- local_description(
    pkg_list = list(
      pkgdepends = "Imports", `dplyr (>= 1.0)` = "Imports"
    ),
    remotes = c("r-lib/pkgdepends@*release")
  )
  expect_true(file.exists(file_path))

  d <- expect_silent(
    desc::desc(file = file_path)
  )

  deps <- d$get_deps()

  expect_true("pkgdepends" %in% deps$package)
  expect_true("dplyr" %in% deps$package)

  expect_equal(deps$version[deps$package == "dplyr"], ">= 1.0")

  expect_equal(NROW(d$get_remotes()), 1)

  ref <- pkgdepends::parse_pkg_ref(d$get_remotes())

  expect_equal(ref$package, "pkgdepends")
  expect_equal(ref$ref, "r-lib/pkgdepends@*release")
  expect_s3_class(ref, "remote_ref_github")
})

test_that("local_description will create a valid DESCRIPTION file with Config/Need/verdepcheck", {
  file_path <- local_description(
    pkg_list = list(
      pkgdepends = "Imports", `dplyr (>= 1.0)` = "Imports"
    ),
    need_verdepcheck = c("r-lib/pkgdepends")
  )
  expect_true(file.exists(file_path))

  d <- expect_silent(
    desc::desc(file = file_path)
  )

  deps <- d$get_deps()

  expect_true("pkgdepends" %in% deps$package)
  expect_true("dplyr" %in% deps$package)

  expect_equal(deps$version[deps$package == "dplyr"], ">= 1.0")

  expect_equal(NROW(d$get_remotes()), 0)

  ref <- pkgdepends::parse_pkg_ref(get_desc_field_pkgs(d))

  expect_equal(ref$package, "pkgdepends")
  expect_equal(ref$ref, "r-lib/pkgdepends")
  expect_s3_class(ref, "remote_ref_github")
})


test_that("local_description will create a temporary file", {
  inner_fun <- function() {
    file_path <- local_description(
      pkg_list = list(pkgdepends = "Imports")
    )

    expect_true(file.exists(file_path))

    d <- desc::desc(file = file_path)

    return(list(d = d, file_path = file_path))
  }

  result <- inner_fun()

  expect_true("pkgdepends" %in% result$d$get_deps()$package)

  expect_false(file.exists(result$file_path))
})

test_that("get_ppm_snapshot_by_date will accept NA", {
  expect_latest_ppm(get_ppm_snapshot_by_date(NA))
})

test_that("get_ppm_snapshot_by_date will accept dates in the future", {
  skip_if_offline()
  auxiliary_fun <- function(days = 0) {
    expect_warning(
      expect_latest_ppm(get_ppm_snapshot_by_date(Sys.Date())),
      "Cannot find PPM snapshot for date"
    )
  }

  auxiliary_fun(0)
  auxiliary_fun(10)
  auxiliary_fun(1)

  expect_failure(
    expect_latest_ppm(get_ppm_snapshot_by_date(Sys.Date() - 365)),
  )
})
