test_that("new_max_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", NULL, NULL)
})

test_that("new_release_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_release_deps_installation_proposal(d_std_path)

  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", NULL, NULL)
})

test_that("new_min_isolated_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.1.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(pkgdepends = "Import"))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.1.0", NULL)
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
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_max_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", NULL, remote_str)
})

test_that("new_release_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_release_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", NULL, NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
})

test_that("new_min_deps_installation_proposal correctly handles <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  remote_str <- "r-lib/pkgdepends"
  d_std_path <- local_description(list(pkgdepends = "Import"), remotes = c(remote_str))
  x <- new_min_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
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

test_that("new_min_isolated_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"), "r-lib/pkgdepends")
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
})

test_that("new_min_isolated_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"))
  x <- new_min_isolated_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles \">=\" dependency for <org>/<repo> reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"), "r-lib/pkgdepends")
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
})

test_that("new_min_cohort_deps_installation_proposal correctly handles \">=\" dependency for standard reference", {
  skip_if_offline()
  skip_if_empty_gh_token()

  d_std_path <- local_description(list(`pkgdepends (>= 0.2.0)` = "Import"))
  x <- new_min_cohort_deps_installation_proposal(d_std_path)
  withr::defer(unlink(x$get_config()$library))

  test_proposal_common(x, "pkgdepends", "source", "0.2.0", NULL)
})
