pkg_name <- "verdepcheck"
library(testthat)
test_check(pkg_name, reporter = ParallelProgressReporter$new())
