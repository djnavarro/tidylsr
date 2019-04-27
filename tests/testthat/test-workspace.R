test_that("workspace returns a tibble", {
  expect_s3_class(object = workspace(), class = "tbl")
  expect_s3_class(object = workspace(), class = "data.frame")
})

test_that("workspace lists objects", {
  expect_equal(object = workspace()[[1]], expected = objects())
  #expect_equal(object = workspace()[[2]], expected = sapply(objects(), function(x) {class(x)[1]}))
  #expect_equal(object = workspace()[[3]], expected = sapply(objects(), length))
})