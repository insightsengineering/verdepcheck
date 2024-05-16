test_that("new_max_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, NULL)
})

test_that("new_release_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_release_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, NULL)
})

test_that("new_min_isolated_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.1.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.1.0", NULL)
})

# #################################################################
#
#            _ _   _                                _
#           (_) | | |                              | |
#  __      ___| |_| |__    _ __ ___ _ __ ___   ___ | |_ ___  ___
#  \ \ /\ / / | __| '_ \  | '__/ _ \ '_ ` _ \ / _ \| __/ _ \/ __|
#   \ V  V /| | |_| | | | | | |  __/ | | | | | (_) | ||  __/\__ \
#    \_/\_/ |_|\__|_| |_| |_|  \___|_| |_| |_|\___/ \__\___||___/
#
#
#
#  with remotes
# ################################################################

test_that("new_max_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  desc_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(
    list(pkgdepends = "Import"),
    remotes = c(remote_str), need_verdepcheck = desc_str
  )
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, remote_str)
})

test_that("new_max_deps_installation_proposal correctly handles <org>/<repo>@*release reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  desc_str <- "r-lib/pkgdepends@*release"
  d_std_path <- local_description(
    list(pkgdepends = "Import"),
    remotes = c(remote_str), need_verdepcheck = desc_str
  )
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, remote_str)
})

test_that("new_max_deps_installation_proposal correctly handles <org>/<repo>@<tag> ref. (particular remote tag)", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends@v0.3.2"
  desc_str <- "r-lib/pkgdepends@v0.3.2"
  d_std_path <- local_description(
    list(pkgdepends = "Import"),
    remotes = c(remote_str), need_verdepcheck = desc_str
  )
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.3.2", remote_str)
})

test_that("new_max_deps_installation_proposal correctly handles <org>/<repo> ref. (without Config/Need/verdpcheck)", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, remote_str)
})

test_that("new_release_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_release_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", NULL, NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.1.0", NULL)
})

test_that("new_min_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  desc_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(
    list(pkgdepends = "Import"),
    remotes = c(remote_str),
    need_verdepcheck = desc_str
  )
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.1.0", NULL)
})

# ################################################################
#
#            _ _   _        ____                   _        __
#           (_) | | |      / /\ \ ______          | |       \ \
#  __      ___| |_| |__   | |  \ \______|   __ _  | |__   ___| |
#  \ \ /\ / / | __| '_ \  | |   > >_____   / _` | | '_ \ / __| |
#   \ V  V /| | |_| | | | | |  / /______| | (_| |_| |_) | (__| |
#    \_/\_/ |_|\__|_| |_| | | /_/          \__,_(_)_.__(_)___| |
#                          \_\                              /_/
#
#
#  with (>= a.b.c)
# ###############################################################

test_that("new_min_isolated_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> ref.", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"), "r-lib/pkgdepends")
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.2.0", NULL)
})

test_that("new_min_isolated_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.2.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"), "r-lib/pkgdepends")
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.2.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "0.2.0", NULL)
})

test_that("new_min_isolated_deps_installation_proposal correctly handles tern and rtables", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(
    list(
      "tern (>= 0.8.3)" = "Import",
      "rtables (>= 0.6.1)" = "Import",
      "formatters (>= 0.5.0)" = "Import"
    ),
    need_verdepcheck = list(
      "insightsengineering/tern",
      "insightsengineering/rtables",
      "insightsengineering/formatters"
    )
  )
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "tern", "0.8.3", NULL)
  test_proposal_common(x, "rtables", "0.6.1", NULL, solve_ip_flag = FALSE)
  test_proposal_common(x, "formatters", "0.5.0", NULL, solve_ip_flag = FALSE)
})

# Test for encapsulation isssue where another dependency (primary or in the tree)
#  requires a version that is more recent than the primary version
#
# Note that the calls to `test_proposal_common` have different versions from the
#  local description specification (in `rtables` and `formatters` packages)
test_that("new_min_isolated_deps_installation_proposal correctly resolves a different version from specifications", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(
    list(
      "tern (>= 0.8.3)" = "Import",
      "rtables (>= 0.6.0)" = "Import",
      "formatters (>= 0.4.1)" = "Import"
    )
  )
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "tern", "0.8.3", NULL)
  test_proposal_common(x, "rtables", "0.6.1", NULL, solve_ip_flag = FALSE)
  test_proposal_common(x, "formatters", "0.5.0", NULL, solve_ip_flag = FALSE)
})

# #################################################################
#
#   ____  _                           _            _
#  |  _ \(_)                         | |          | |
#  | |_) |_  ___   ___ ___  _ __   __| |_   _  ___| |_ ___  _ __
#  |  _ <| |/ _ \ / __/ _ \| '_ \ / _` | | | |/ __| __/ _ \| '__|
#  | |_) | | (_) | (_| (_) | | | | (_| | |_| | (__| || (_) | |
#  |____/|_|\___/ \___\___/|_| |_|\__,_|\__,_|\___|\__\___/|_|
#
#
#
#  Install Bioconductor single packages
#
#  note: until pkgcache or another mechanism can retrieve the published date
#   of Bioconductor packages some warnings are expected as it uses the current
#   date instead. Bioconductor packges have their versions bumped every 6 months
#   with a new Bioc release.
# ################################################################


test_that("new_min_cohort_deps_installation_proposal correctly handles Bioc package", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(SummarizedExperiment = "Import"))

  expect_warning(
    x <- new_min_cohort_deps_installation_proposal(d_std_path),
    "Cannot find PPM snapshot"
  )

  withr::defer(unlink(x$get_config()$library))

  test_proposal_common_bioc(x, "SummarizedExperiment")
})

test_that("new_min_isolated_deps_installation_proposal correctly handles Bioc package", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(SummarizedExperiment = "Import"))

  x <- new_min_isolated_deps_installation_proposal(d_std_path)

  withr::defer(unlink(x$get_config()$library))

  expect_warning(
    test_proposal_common_bioc(x, "SummarizedExperiment"),
    "Cannot find PPM snapshot"
  )
})

test_that("new_release_deps_installation_proposal correctly handles Bioc package", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(SummarizedExperiment = "Import"))

  x <- new_release_deps_installation_proposal(d_std_path)

  withr::defer(unlink(x$get_config()$library))

  test_proposal_common_bioc(x, "SummarizedExperiment")
})

test_that("new_max_deps_installation_proposal correctly handles Bioc package", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(SummarizedExperiment = "Import"))

  x <- new_max_deps_installation_proposal(d_std_path)

  withr::defer(unlink(x$get_config()$library))

  test_proposal_common_bioc(x, "SummarizedExperiment")
})
