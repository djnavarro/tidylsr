

new_tbl <- function(n = 20) {
  tibble(
    out = rnorm(n = 2*n) + rep(c(0,1), n),
    grp = rep(c("time 1", "time 2"), n),
    id = as.character(paste0("subj", gl(n, 2)))
  )
}

test_that("print method for lsr_ttest returns the original object", {

  tbl <- new_tbl()
  tt <- ttest_twosample(tbl, out ~ grp)

  expect_equal(tt, print(tt))
})


test_that("output from a t-test has the correct number of lines", {

  tbl <- new_tbl()
  tt1 <- ttest_twosample(tbl, out ~ grp)
  tt2 <- ttest_paired(tbl, out ~ grp + (id))
  tt3 <- ttest_onesample(tbl, outcome = out, null_mean = 0)

  tp1 <- capture.output(print(tt1))
  tp2 <- capture.output(print(tt2))
  tp3 <- capture.output(print(tt3))

  expect_length(tp1, 27)
  expect_length(tp2, 29)
  expect_length(tp3, 25)
})

