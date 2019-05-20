
new_tbl <- function(n = 20) {
  tibble(
    out = rnorm(n = 2*n) + rep(c(0,1), n),
    grp = rep(c("time 1", "time 2"), n),
    id = as.character(paste0("subj", gl(n, 2)))
  )
}

test_that("formula and named arguments to ttest_twosample give the same answer", {

  tbl <- new_tbl()
  tt1 <- ttest_twosample(tbl, outcome = out, group = grp)
  tt2 <- ttest_twosample(tbl, formula = out ~ grp)
  tt3 <- ttest_twosample(tbl, out ~ grp)

  expect_equal(tt1, tt2)
  expect_equal(tt2, tt3)

})

test_that("formula and named args of ttest_paired give the same answer", {

  tbl <- new_tbl()
  tt1 <- ttest_paired(tbl, outcome = out, group = grp, id = id)
  tt2 <- ttest_paired(tbl, formula = out ~ grp + (id))
  tt3 <- ttest_paired(tbl, out ~ grp + (id))
  tt4 <- ttest_paired(tbl, out ~ (id) + grp)

  expect_equal(tt1, tt2)
  expect_equal(tt2, tt3)
  expect_equal(tt3, tt4)

})



test_that("one and two-sided p-values add appropriately", {

  tbl <- new_tbl()
  tt1 <- ttest_twosample(tbl, out ~ grp, test_greater = "time 1")
  tt2 <- ttest_twosample(tbl, out ~ grp, test_greater = "time 2")
  tt3 <- ttest_twosample(tbl, out ~ grp)

  p1 <- tt1$test$p
  p2 <- tt2$test$p
  p3 <- tt3$test$p

  expect_equal(p1 + p2, 1)
  expect_equal(min(p1,p2), p3/2)

})

new_ttlist <- function(...) {

  tbl <- new_tbl()
  tt <- list()

  tt[[1]] <- ttest_twosample(tbl, outcome = out, group = grp)
  tt[[2]] <- ttest_twosample(tbl, formula = out ~ grp)
  tt[[3]] <- ttest_twosample(tbl, out ~ grp)
  tt[[4]] <- ttest_twosample(tbl, out ~ grp, equal_variances = TRUE)
  tt[[5]] <- ttest_paired(tbl, outcome = out, group = grp, id = id)
  tt[[6]] <- ttest_paired(tbl, formula = out ~ grp + (id))
  tt[[7]] <- ttest_paired(tbl, out ~ grp + (id))
  tt[[8]] <- ttest_paired(tbl, out ~ (id) + grp)
  tt[[9]] <- ttest_onesample(tbl, outcome = out, null_mean = 1)

  tt[[10]] <- ttest_twosample(tbl, outcome = out, group = grp, test_greater = "time 1")
  tt[[11]] <- ttest_twosample(tbl, formula = out ~ grp, test_greater = "time 1")
  tt[[12]] <- ttest_twosample(tbl, out ~ grp, test_greater = "time 1")
  tt[[13]] <- ttest_twosample(tbl, out ~ grp, equal_variances = TRUE, test_greater = "time 1")
  tt[[14]] <- ttest_paired(tbl, outcome = out, group = grp, id = id, test_greater = "time 1")
  tt[[15]] <- ttest_paired(tbl, formula = out ~ grp + (id), test_greater = "time 1")
  tt[[16]] <- ttest_paired(tbl, out ~ grp + (id), test_greater = "time 1")
  tt[[17]] <- ttest_paired(tbl, out ~ (id) + grp, test_greater = "time 1")
  tt[[18]] <- ttest_onesample(tbl, outcome = out, null_mean = 1, test_greater = TRUE)

  tt[[19]] <- ttest_twosample(tbl, outcome = out, group = grp, test_greater = "time 2")
  tt[[20]] <- ttest_twosample(tbl, formula = out ~ grp, test_greater = "time 2")
  tt[[21]] <- ttest_twosample(tbl, out ~ grp, test_greater = "time 2")
  tt[[22]] <- ttest_twosample(tbl, out ~ grp, equal_variances = TRUE, test_greater = "time 2")
  tt[[23]] <- ttest_paired(tbl, outcome = out, group = grp, id = id, test_greater = "time 2")
  tt[[24]] <- ttest_paired(tbl, formula = out ~ grp + (id), test_greater = "time 2")
  tt[[25]] <- ttest_paired(tbl, out ~ grp + (id), test_greater = "time 2")
  tt[[26]] <- ttest_paired(tbl, out ~ (id) + grp, test_greater = "time 2")
  tt[[27]] <- ttest_onesample(tbl, outcome = out, null_mean = 1, test_greater = FALSE)


  return(tt)
}


test_that("variables is always a 1x6 tibble", {

  tt <- new_ttlist()

  for(i in 1:length(tt)) {
    expect_s3_class(tt[[i]]$variables, "tbl_df")
    expect_equal(nrow(tt[[i]]$variables), 1)
    expect_equal(ncol(tt[[i]]$variables), 6)
    expect_named(tt[[i]]$variables,
                 c("outcome", "group", "id", "sample1", "sample2", "null_mean"))
    expect_type(tt[[i]]$variables$outcome, "character")
    expect_type(tt[[i]]$variables$group, "character")
    expect_type(tt[[i]]$variables$id, "character")
    expect_type(tt[[i]]$variables$sample1, "character")
    expect_type(tt[[i]]$variables$sample2, "character")
    expect_type(tt[[i]]$variables$null_mean, "double")
  }

})


test_that("variables is always a 3-column tibble", {

  tt <- new_ttlist()

  for(i in 1:length(tt)) {
    expect_s3_class(tt[[i]]$descriptives, "tbl_df")
    expect_equal(ncol(tt[[i]]$descriptives), 3)
    expect_named(tt[[i]]$descriptives, c("sample", "mean", "sd"))
    expect_type(tt[[i]]$descriptives$mean, "double")
    expect_type(tt[[i]]$descriptives$sd, "double")
  }

})




test_that("test is always a 1x8 tibble", {

  tt <- new_ttlist()

  for(i in 1:length(tt)) {
    expect_s3_class(tt[[i]]$test, "tbl_df")
    expect_equal(nrow(tt[[i]]$test), 1)
    expect_equal(ncol(tt[[i]]$test), 8)
    expect_named(tt[[i]]$test, c("type", "hypotheses", "t", "df", "p",
                                 "ci_lower", "ci_upper", "ci_level"))
    expect_type(tt[[i]]$test$type, "character")
    expect_type(tt[[i]]$test$hypotheses, "character")
    expect_type(tt[[i]]$test$t, "double")
    expect_type(tt[[i]]$test$df, "double")
    expect_type(tt[[i]]$test$p, "double")
    expect_type(tt[[i]]$test$ci_lower, "double")
    expect_type(tt[[i]]$test$ci_upper, "double")
    expect_type(tt[[i]]$test$ci_level, "double")
  }

})




