test_that("show_environment returns a tibble", {
  expect_s3_class(object = show_environment(), class = "tbl")
  expect_s3_class(object = show_environment(), class = "data.frame")
})

test_that("show_environment lists objects", {
  expect_equal(object = show_environment()[[1]], expected = objects())
})