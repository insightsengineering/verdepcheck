test_that("get_release_date.remote_ref_github will only retrieve 1 date for teal@v0.10.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  # Teal v0.10.0 has 2 tags (release candidate and release)
  remote_ref <- pkgdepends::parse_pkg_ref("insightsengineering/teal@v0.10.0")
  result <- get_release_date.remote_ref_github(remote_ref)

  expect_length(result, 1)
  expect_identical(result, "2021-10-08T15:10:35Z")
})

test_that("get_release_date.remote_ref_github will only retrieve 1 date for rlang@1.0.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  # Teal v0.10.0 has 2 tags (release candidate and release)
  remote_ref <- pkgdepends::parse_pkg_ref("r-lib/rlang@v1.0.0")
  result <- get_release_date.remote_ref_github(remote_ref)

  expect_length(result, 1)
  expect_identical(result, "2022-01-20T16:47:02Z")
})

test_that("get_release_date.remote_ref_github will only retrieve 1 date for rlang@0.0.0", {
  skip_if_offline()
  skip_if_empty_gh_token()

  # Teal v0.10.0 has 2 tags (release candidate and release)
  remote_ref <- pkgdepends::parse_pkg_ref("r-lib/rlang@v0.0.0")
  result <- get_release_date.remote_ref_github(remote_ref)

  expect_length(result, 1)
  expect_true(is.na(result))
})
