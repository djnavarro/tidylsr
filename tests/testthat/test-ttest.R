tbl <- tibble(
  out = rnorm(n = 40) + rep(c(0,1), 20),
  grp = rep(c("grp A", "grp B"), 20) # names with spaces
)


test_that("all three versions give the same answer", {

  tst1 <- ttest_twosample(tbl, outcome = out, group = grp)
  tst2 <- ttest_twosample(tbl, formula = out ~ grp)
  tst3 <- ttest_twosample(tbl, out ~ grp)

  expect_equal(tst1, tst2)
  expect_equal(tst2, tst3)

})

test_that("one and two-sided p-values add appropriately", {

  tst1 <- ttest_twosample(tbl, out ~ grp, alternative = "grp A" < "grp B")
  tst2 <- ttest_twosample(tbl, out ~ grp, alternative = "grp B" < "grp A")
  tst3 <- ttest_twosample(tbl, out ~ grp)


  p1 <- tst1$p
  p2 <- tst2$p
  p3 <- tst3$p

  expect_equal(p1 + p2, 1)
  expect_equal(min(p1,p2), p3/2)

})


# if(FALSE) {
#   iris %>%
#     janitor::clean_names() %>%
#     filter(species != "versicolor") %>%
#     ttest_twosample(sepal_length ~ species,
#                     alternative = "virginica" > "setosa")
# }
