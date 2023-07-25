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
