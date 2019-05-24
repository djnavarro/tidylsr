test_that("ttest_handle_missing works", {

  tbl <- tibble::tibble(
    x = c(1,    2,  NA,  NaN,   5),
    y = c("y", NA,  "n", "y", "n"),
    z = c(NA,   1,   2,    3,   4)
  )

  expect_warning(ttest_handle_missing(tbl, "x"), "2 observations removed")
  expect_warning(ttest_handle_missing(tbl, "y"), "1 observations removed")
  expect_warning(ttest_handle_missing(tbl, "z"), "1 observations removed")
  expect_warning(ttest_handle_missing(tbl, c("x", "y")), "3 observations removed")
  expect_warning(ttest_handle_missing(tbl, c("x", "z")), "3 observations removed")
  expect_warning(ttest_handle_missing(tbl, c("y", "z")), "2 observations removed")
  expect_warning(ttest_handle_missing(tbl, c("x", "y", "z")), "4 observations removed")

  expect_equal(dim(ttest_handle_missing(tbl, "x", TRUE)), c(3, 1))
  expect_equal(dim(ttest_handle_missing(tbl, "y", TRUE)), c(4, 1))
  expect_equal(dim(ttest_handle_missing(tbl, "z", TRUE)), c(4, 1))
  expect_equal(dim(ttest_handle_missing(tbl, c("x", "y"), TRUE)), c(2, 2))
  expect_equal(dim(ttest_handle_missing(tbl, c("x", "z"), TRUE)), c(2, 2))
  expect_equal(dim(ttest_handle_missing(tbl, c("y", "z"), TRUE)), c(3, 2))
  expect_equal(dim(ttest_handle_missing(tbl, c("x", "y", "z"), TRUE)), c(1, 3))

})
