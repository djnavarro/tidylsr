test_that("show_workspace returns a tibble", {
  expect_s3_class(object = show_workspace(), class = "tbl")
  expect_s3_class(object = show_workspace(), class = "data.frame")
})

test_that("show_workspace lists objects", {
  expect_equal(object = show_workspace()[[1]], expected = objects())
})