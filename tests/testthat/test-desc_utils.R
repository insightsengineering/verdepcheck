test_that("filter_valid_version will filter for valid versions", {
  versions <- paste(1:10, rep("0", 10), sep = ".")

  expect_length(filter_valid_version(versions, ">=", "3.1"), 7)

  expect_length(filter_valid_version(versions, ">", "3.1"), 7)
  expect_length(filter_valid_version(versions, ">", "3.0"), 7)

  expect_length(filter_valid_version(versions, "<=", "2.0"), 2)

  expect_length(filter_valid_version(versions, "<", "2.1"), 2)
  expect_length(filter_valid_version(versions, "<", "2.0.0"), 1)

  expect_length(filter_valid_version(versions, "==", "3.1"), 0)
  expect_length(filter_valid_version(versions, "==", "1.0.0.0.0.0.0"), 1)

  expect_length(filter_valid_version(versions, "!=", "1.0"), 9)
  expect_length(filter_valid_version(versions, "!=", "1.0.01"), 10)
})

test_that("check_valid_version will return vector of logicals", {
  versions <- paste(1:10, rep("0", 10), sep = ".")

  expect_true(all(check_valid_version(versions, "", "")))
  expect_true(all(check_valid_version(versions, ">=", "")))
  expect_true(all(check_valid_version(versions, ">=", "0.0.0")))

  expect_error(
    check_valid_version(versions, "0.3", ">="),
    "invalid version specification"
  )

  expect_error(
    check_valid_version(versions, ">="),
    "argument \"op_ver\" is missing, with no default"
  )

  expect_error(
    check_valid_version(versions),
    "argument \"op\" is missing, with no default"
  )
})

test_that("desc_remotes_cleanup will replace remotes with tag", {
  d <- desc::desc(
    file = verdepcheck:::local_description(
      list(
        dplyr = "Import",
        "tibble" = "Import",
        pkgdepends = "Import"
      ),
      remotes = c(
        "tidyverse/dplyr@*release",
        "tidyverse/tibble@*release",
        "r-lib/pkgdepends@*release"
      ),
      need_verdepcheck = c(
        "dplyr",
        "tibble=tidyverse/tibble@v3.2.1"
      )
    )
  )

  clean_d <- desc_remotes_cleanup(d)

  expect_contains(clean_d$get_remotes(), "r-lib/pkgdepends@*release")
  expect_failure(expect_contains(clean_d$get_remotes(), "tibble=tidyverse/tibble@v3.2.1"))
  expect_failure(expect_contains(clean_d$get_remotes(), "tidyverse/dplyr@*release"))
})

test_that("desc_remotes_cleanup will remove Config/Needs/verdepcheck's CRAN reference from remotes", {
  d <- desc::desc(
    file = verdepcheck:::local_description(
      list(
        dplyr = "Import",
        "tibble" = "Import",
        pkgdepends = "Import"
      ),
      remotes = c(
        "tidyverse/dplyr@*release",
        "r-lib/pkgdepends@*release"
      ),
      need_verdepcheck = c(
        "dplyr",
        "tibble=tidyverse/tibble@v3.2.1"
      )
    )
  )

  clean_d <- desc_remotes_cleanup(d)

  expect_length(clean_d$get_remotes(), 1)
  expect_contains(clean_d$get_remotes(), "r-lib/pkgdepends@*release")
  expect_failure(expect_contains(clean_d$get_remotes(), "tibble=tidyverse/tibble@v3.2.1"))
  expect_failure(expect_contains(clean_d$get_remotes(), "tidyverse/dplyr@*release"))
  expect_failure(expect_contains(clean_d$get_remotes(), "tidyverse/dplyr"))
  expect_failure(expect_contains(clean_d$get_remotes(), "dplyr"))
})

test_that("desc_remotes_cleanup will not add to remotes", {
  d <- desc::desc(
    file = verdepcheck:::local_description(
      list(
        dplyr = "Import",
        "tibble" = "Import",
        pkgdepends = "Import"
      ),
      remotes = c(
        "tidyverse/dplyr@*release",
        "r-lib/pkgdepends@*release"
      ),
      need_verdepcheck = c(
        "tidyverse/dplyr@v1.0.0",
        "tibble=tidyverse/tibble@v3.2.1"
      )
    )
  )

  clean_d <- desc_remotes_cleanup(d)

  expect_length(clean_d$get_remotes(), 1)
  expect_contains(clean_d$get_remotes(), "r-lib/pkgdepends@*release")
  expect_failure(expect_contains(clean_d$get_remotes(), "tibble=tidyverse/tibble@v3.2.1"))
  expect_failure(expect_contains(clean_d$get_remotes(), "tidyverse/dplyr@v1.0.0"))
})

test_that("desc_remotes_cleanup accepts no Config/Need/verdepcheck", {
  d <- desc::desc(
    file = verdepcheck:::local_description(
      list(
        dplyr = "Import",
        "tibble" = "Import",
        pkgdepends = "Import"
      ),
      remotes = c(
        "tidyverse/dplyr@*release",
        "tidyverse/tibble@*release",
        "r-lib/pkgdepends@*release"
      )
    )
  )

  clean_d <- desc_remotes_cleanup(d)

  expect_length(get_desc_field_pkgs(clean_d), 0)

  expect_length(clean_d$get_remotes(), 3)
  expect_contains(clean_d$get_remotes(), "r-lib/pkgdepends@*release")
  expect_contains(clean_d$get_remotes(), "tidyverse/tibble@*release")
  expect_contains(clean_d$get_remotes(), "tidyverse/dplyr@*release")
})

test_that("get_desc_field_pkgs allows for no Config/Needs/verdepcheck", {
  d <- desc::desc(
    file = verdepcheck:::local_description(
      list(
        dplyr = "Import",
        "tibble" = "Import",
        pkgdepends = "Import"
      ),
      remotes = c(
        "tidyverse/dplyr@*release",
        "tidyverse/tibble@*release",
        "r-lib/pkgdepends@*release"
      )
    )
  )

  expect_length(get_desc_field_pkgs(d), 0)
})
