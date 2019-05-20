tbl <- tibble(
  out = rnorm(n = 40) + rep(c(0,1), 20),
  grp = rep(c("grp A", "grp B"), 20) # names with spaces
)


test_that("all three versions of ttest_twosample give the same answer", {

  tst1 <- ttest_twosample(tbl, outcome = out, group = grp)
  tst2 <- ttest_twosample(tbl, formula = out ~ grp)
  tst3 <- ttest_twosample(tbl, out ~ grp)

  expect_equal(tst1, tst2)
  expect_equal(tst2, tst3)

})

test_that("one and two-sided p-values add appropriately", {

  tst1 <- ttest_twosample(tbl, out ~ grp, test_greater = "grp A")
  tst2 <- ttest_twosample(tbl, out ~ grp, test_greater = "grp B")
  tst3 <- ttest_twosample(tbl, out ~ grp)

  p1 <- tst1$test$p
  p2 <- tst2$test$p
  p3 <- tst3$test$p

  expect_equal(p1 + p2, 1)
  expect_equal(min(p1,p2), p3/2)

})


tbl2 <- tibble(
  out = rnorm(n = 40) + rep(c(0,1), 20),
  grp = rep(c("time 1", "time 2"), 20),
  id = as.character(paste0("subj", gl(20, 2)))
)


test_that("all versions of ttest_paired give the same answer", {

  tst1 <- ttest_paired(tbl2, outcome = out, group = grp, id = id)
  tst2 <- ttest_paired(tbl2, formula = out ~ grp + (id))
  tst3 <- ttest_paired(tbl2, out ~ grp + (id))
  tst4 <- ttest_paired(tbl2, out ~ (id) + grp)

  expect_equal(tst1, tst2)
  expect_equal(tst2, tst3)
  expect_equal(tst3, tst4)

})

