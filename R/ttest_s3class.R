

# Constructor function for the lsr_ttest class. At the moment
# it is just ridiculous duplication but I suspect I'll want to
# use it to check inputs in some cases?
new_lsr_ttest <- function(outcome = NULL, group = NULL, id = NULL, t = NULL,
                          df = NULL, p = NULL, conf_int = NULL, conf_lvl = NULL,
                          sample_mean = NULL, sample_sd = NULL, group_name = NULL,
                          alternative = NULL, test_type = NULL, null_mean = NULL,
                          effect_size = NULL) {

  structure(
    list(
      outcome = outcome,
      group = group,
      id = id,
      t = t,
      df = df,
      p = p,
      conf_int = conf_int,
      conf_lvl = conf_lvl,
      sample_mean = sample_mean,
      sample_sd = sample_sd,
      group_name = group_name,
      alternative = alternative,
      test_type = test_type,
      null_mean = null_mean,
      effect_size = effect_size
    ),
    class = "lsr_ttest"
  )

}

# Specify a print method for the t-test
#' @export
print.lsr_ttest <- function(x, digits = 3, ...) {

  # round to the default number of digits
  round_def <- function(x) round(x, digits)

  # print the name of the test
  test_str <- switch(x$test_type,
                     "One sample" = "One sample t-test",
                     "Student" = "Student's two sample t-test",
                     "Welch" = "Welch's two sample t-test",
                     "Paired" = "Paired samples t-test"
  )
  cat("\n  ", test_str, "\n\n")

  # print the names of the variables
  cat("Variables: \n")
  if(!is.null(x$outcome)) cat("   outcome: ", x$outcome, "\n")
  if(!is.null(x$group)) cat("   group:   ", x$group, "\n")
  if(!is.null(x$id)) cat("   id:      ", x$id, "\n")
  cat("\n")

  # print the table of descriptive statistics
  descriptives <- rbind(x$sample_mean, x$sample_sd )
  rownames(descriptives) <- c("   mean","   std dev.")
  colnames(descriptives) <- switch(x$test_type,
                                   "One sample" = x$outcome,
                                   "Student" = x$group_name,
                                   "Welch" = x$group_name,
                                   "Paired" = c(x$group_name, "diff.")
  )
  txt_mat <- function(x, fmt = paste0("%.", digits, "f")) {
    matrix(sprintf(fmt, x), nrow(x), ncol(x), dimnames = dimnames(x))
  }
  cat("Descriptives: \n")
  print(txt_mat(descriptives), quote = FALSE, right = TRUE)
  cat("\n")

  # print the hypotheses being tested
  hyp <- get_verbose_hypotheses(x)
  cat("Hypotheses: \n")
  cat("    null:        ", hyp["null"], "\n")
  cat("    alternative: ", hyp["altr"], "\n")
  cat("\n")

  # print the test results
  cat("Test results: \n")
  cat("   t-statistic:        ", round_def(x$t), "\n")
  cat("   degrees of freedom: ", round_def(x$df), "\n")
  cat("   p-value:            ", ifelse(x$p<.001, "<.001", round_def(x$p)), "\n")
  cat("\n")

  # print the confidence interval
  cat(round(x$conf_lvl * 100), "% confidence interval:", "\n", sep = "")
  cat("   lower bound: ", round_def(x$conf_int[1]), "\n")
  cat("   upper bound: ", round_def(x$conf_int[2]), "\n")
  cat("\n")

}

