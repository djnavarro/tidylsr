

# Constructor function for the lsr_ttest class. At the moment
# it is just ridiculous duplication but I suspect I'll want to
# use it to check inputs in some cases?
new_lsr_ttest <- function(variables, test, descriptives) {

  structure(
    list(
      variables = variables,
      test = test,
      descriptives = descriptives
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
  test_str <- switch(x$test$type,
                     "one_sample" = "One sample t-test",
                     "student" = "Student's two sample t-test",
                     "welch" = "Welch's two sample t-test",
                     "paired" = "Paired samples t-test"
  )
  cat("\n   ", test_str, "\n\n")

  # print the names of the variables
  cat("Variables: \n")
  if(!is.na(x$variables$outcome)) cat("    outcome: ", x$variables$outcome, "\n")
  if(!is.na(x$variables$group)) cat("    group:   ", x$variables$group, "\n")
  if(!is.na(x$variables$id)) cat("    id:      ", x$variables$id, "\n")
  cat("\n")

  # print the table of descriptive statistics
  cat("Descriptives: \n")
  desc <- lapply(x$descriptives, function(x) {
    format(x, width = 10, digits = 3, nsmall = 3, justify = "right")
  })
  mk_pad <- function(n) {paste0(rep(" ", max(c(0, n))), collapse="")}
  ttl_pad <- mk_pad(nchar(x$descriptives$sample)-10)
  lft_pad <- mk_pad(min(4, max(nchar(x$descriptives$sample)-10)))
  cat(lft_pad, ttl_pad, "    sample", "      mean", "        sd", "\n", sep="")
  for(i in 1:nrow(x$descriptives)) {
    cat(lft_pad, desc[[1]][i], desc[[2]][i], desc[[3]][i], "\n", sep="")
  }
  cat("\n")

  # print the hypotheses being tested
  hyp <- get_verbose_hypotheses(x)
  cat("Hypotheses: \n")
  cat("    null:        ", hyp["null"], "\n")
  cat("    alternative: ", hyp["altr"], "\n")
  cat("\n")

  # print the test results
  cat("Test results: \n")
  cat("    t-statistic:        ", round_def(x$test$t), "\n")
  cat("    degrees of freedom: ", round_def(x$test$df), "\n")
  cat("    p-value:            ", ifelse(x$test$p<.001, "<.001", round_def(x$test$p)), "\n")
  cat("\n")

  # print the confidence interval
  cat(round(x$test$ci_level * 100), "% confidence interval:", "\n", sep = "")
  cat("    lower bound: ", round_def(x$test$ci_lower), "\n")
  cat("    upper bound: ", round_def(x$test$ci_upper), "\n")
  cat("\n")

  return(invisible(x))

}

