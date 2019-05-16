tbl <- tibble(
  out = rnorm(n = 20),
  grp = sample(c("A","B"), size = 20, replace = TRUE)
)


test_that("all three versions give the same answer", {

  tst1 <- ttest_twosample(tbl, outcome = out, group = grp)
  tst2 <- ttest_twosample(tbl, formula = out ~ grp)
  tst3 <- ttest_twosample(tbl, out ~ grp)

  expect_equal(tst1, tst2)
  expect_equal(tst2, tst3)

})
