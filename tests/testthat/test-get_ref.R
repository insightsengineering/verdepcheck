test_that("get_release_date.remote_ref_github will only retrieve 1 date for teal@v0.10.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  # Teal v0.10.0 has 2 tags (release candidate and release)
  remote_ref <- pkgdepends::parse_pkg_ref("insightsengineering/teal@v0.10.0")
  result <- get_release_date.remote_ref_github(remote_ref)

  expect_length(result, 1)
  expect_s3_class(result, "Date")
  expect_identical(as.Date(result), as.Date("2021-10-08T15:10:35Z"))
})

test_that("get_release_date.remote_ref_github will only retrieve 1 date for rlang@1.0.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_ref <- pkgdepends::parse_pkg_ref("r-lib/rlang@v1.0.0")
  result <- get_release_date.remote_ref_github(remote_ref)

  expect_length(result, 1)
  expect_s3_class(result, "Date")
  expect_identical(as.Date(result), as.Date("2022-01-20T16:47:02Z"))
})

test_that("get_release_date.remote_ref_github will retrieve missing date (NA) for r-lib/rlang@v0.0.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_ref <- pkgdepends::parse_pkg_ref("r-lib/rlang@v0.0.0")

  # https://github.com/r-lib/pkgdepends/issues/365
  withr::with_options(
    opts_partial_match_old,
    result <- get_release_date.remote_ref_github(remote_ref)
  )

  expect_length(result, 1)
  expect_true(is.na(result))
  expect_s3_class(result, "Date")
})

test_that("get_release_date.remote_ref_cran will retrieve missing date (NA) for package.does.not.exist@1.1.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_ref <- pkgdepends::parse_pkg_ref("package.does.not.exist@1.1.0")
  result <- get_release_date.remote_ref_cran(remote_ref)

  expect_length(result, 1)
  expect_true(is.na(result))
  expect_s3_class(result, "Date")
})

test_that("get_release_date with any class other than remote_ref.{github,cran,standard} returns missing", {
  remote_ref <- pkgdepends::parse_pkg_ref("dplyr@1.1.0")
  class(remote_ref) <- Filter(
    function(el) !grepl("remote_ref_(cran|github|standard)", el),
    class(remote_ref)
  )
  result <- get_release_date.remote_ref(remote_ref)

  expect_length(result, 1)
  expect_true(is.na(result))
  expect_s3_class(result, "Date")
})

test_that("get_release_data returns date for Bioconductor", {
  skip_if_offline()

  result <- get_release_data("MultiAssayExperiment")$mtime
  expect_length(result, 1)
  expect_s3_class(result, "POSIXct")
})

test_that("get_ref_release returns a CRAN remote_reference if package exists", {
  skip_if_offline()
  skip_if_empty_gh_token()

  test_refs <- c(
    "dplyr",
    "dplyr@1.1.0",
    "tidyverse/dplyr",
    "tidyverse/dplyr@v1.1.0",
    "tidyverse/dplyr@c48230c13"
  )

  for (el_ref in test_refs) {
    remote_ref <- pkgdepends::parse_pkg_ref(el_ref)
    expect_s3_class(get_ref_release(remote_ref), "remote_ref_standard")
  }
})

test_that("get_ref_release returns a CRAN remote_reference if package exists", {
  skip_if_offline()
  skip_if_empty_gh_token()

  testthat::local_mocked_bindings(
    check_if_on_cran = function(remote_ref, op = "", op_ver = "") FALSE
  )

  test_refs <- c(
    "tidyverse/dplyr", # github format
    "tidyverse/dplyr@c48230c13", # commit format
    "tidyverse/dplyr#681", # pull request format
    "tidyverse/dplyr@v1.1.0", # tag format
    "tidyverse/dplyr@*release" # release format
  )

  for (el_ref in test_refs) {
    remote_ref <- pkgdepends::parse_pkg_ref(el_ref)
    expect_s3_class(get_ref_release(remote_ref), "remote_ref_github")
  }
})
