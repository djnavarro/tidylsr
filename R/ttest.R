
#' Two samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param big_alternative value of specifying which group is larger under the alternative, or NULL (default) to specify a two-sided test
#' @param equal_variances should the test assume equality of variance? (default = FALSE)
#' @param ... other arguments to be passed to t.test
#' @export
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL,
                            big_alternative = NULL, equal_variances = FALSE, ...) {

  # outcome and group as expressions
  if(is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
  } else {
    outcome <- formula[[2]]
    group <- formula[[3]]
  }

  # outcome and group as strings
  outcome <- as.character(outcome)
  group <- as.character(group)

  # extract group names and samples
  grp_names <- get_group_names(data[[group]])
  x <- data[[outcome]][data[[group]] == grp_names[1]]
  y <- data[[outcome]][data[[group]] == grp_names[2]]

  # specify the alternative hypothesis
  alt <- get_direction(big_alternative, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = alt,
                         var.equal = equal_variances, ...)

  # format the output
  out <- new_lsr_ttest(
    outcome = outcome,
    group = group,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    conf_int = strip(ttest$conf.int),
    conf_lvl = attr(ttest$conf.int, "conf.level"),
    sample_mean = c(mean(x), mean(y)),
    sample_sd = c(stats::sd(x), stats::sd(y)),
    group_name = grp_names,
    alternative = alt,
    test_type = ifelse(equal_variances, "Student", "Welch")
  )

  return(out)
}

# specify the test direction
get_direction <- function(big_alt, grp_names) {
  if(is.null(big_alt)) return("two.sided")
  if(big_alt == grp_names[1]) return("greater")
  if(big_alt == grp_names[2]) return("less")
  stop("`big_alternative` must be NULL or a value indicating a group",
       call. = FALSE)
}

# extract group names (and don't return a factor)
get_group_names <- function(grp) {
  grp_names <- unique(grp)
  if(is.factor(grp_names)) grp_names <- as.character(grp_names)
  return(grp_names)
}


#' Paired samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param id the id variable (qouoted)
#' @param big_alternative value of specifying which group is larger under the alternative, or NULL (default) to specify a two-sided test
#' @param ... other arguments to be passed to t.test
#' @importFrom dplyr %>%
#' @export
ttest_paired <- function(data, formula = NULL, outcome = NULL, group = NULL,
                         id = NULL, big_alternative = NULL,  ...) {

  # outcome, group, id as expressions
  if(is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
    id <- rlang::enexpr(id)
  } else {
    outcome <- formula[[2]]
    rhs <- formula[[3]]
    rhs_vars <- c(rhs[[2]], rhs[[3]])
    id_ind <- grep("^\\(.*\\)$", rhs_vars)
    group <- rhs_vars[[3-id_ind]]
    id <- rhs_vars[[id_ind]][2] # element 1 is "(", element 2 is var name
  }

  # outcome and group can just be strings
  outcome <- as.character(outcome)
  group <- as.character(group)
  id <- as.character(id)

  # extract group names
  grp_names <- get_group_names(data[[group]])

  # create a wide form version of the data
  wide_data <- data %>%
    dplyr::select(!!id, !!group, !!outcome) %>%
    tidyr::spread(key = !!group, value = !!outcome)

  # extract the paired samples
  x <- wide_data[[grp_names[1]]]
  y <- wide_data[[grp_names[2]]]

  # specify the alternative hypothesis
  alt <- get_direction(big_alternative, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = alt, paired = TRUE, ...)

  # format the output
  out <- new_lsr_ttest(
    outcome = outcome,
    group = group,
    id = id,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    conf_int = strip(ttest$conf.int),
    conf_lvl = attr(ttest$conf.int, "conf.level"),
    sample_mean = c(mean(x), mean(y), mean(x-y)),
    sample_sd = c(stats::sd(x), stats::sd(y), stats::sd(x-y)),
    group_name = grp_names,
    alternative = alt,
    test_type = "Paired"
  )

  return(out)
}


#' One sample t-test
#'
#' @param data a data frame or tibble
#' @param outcome the outcome variable (quoted)
#' @param null_mean the fixed mean to test against (numeric)
#' @param alternative character specifying "two.sided" (the default), "greater", or "less"
#' @param ... other arguments to be passed to t.test
#' @export
ttest_onesample <- function(data, outcome = NULL, null_mean = NULL,
                            alternative = "two.sided", ...) {

  # extract outcome variable as character string
  outcome <- as.character(rlang::enexpr(outcome))

  # extract the sample
  x <- data[[outcome]]

  # run the t-test
  ttest <- stats::t.test(x=x, mu=null_mean, alternative = alternative, ...)

  # format the output
  out <- new_lsr_ttest(
    outcome = outcome,
    null_mean = null_mean,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    conf_int = strip(ttest$conf.int),
    conf_lvl = attr(ttest$conf.int, "conf.level"),
    sample_mean = mean(x),
    sample_sd = stats::sd(x),
    alternative = alternative,
    test_type = "One sample"
  )

  return(out)
}



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


get_verbose_hypotheses <- function(x) {

  tt <- switch(
    x$test_type,
    "One sample" = "one",
    "Student" = "two",
    "Welch" = "two",
    "Paired" = "two"
  )

  # --- one sample test hypotheses ---

  if(tt == "one" & x$alternative == "two.sided") {
    return(c(
      null = paste0("population mean equals", x$null_mean),
      altr = paste0("population mean not equal to", x$null_mean)
    ))
  }

  if(tt == "one" & x$alternative == "greater") {
    return(c(
      null = paste0("population mean less than or equal to", x$null_mean),
      altr = paste0("population mean greater than", x$null_mean)
    ))
  }

  if(tt == "one" & x$alternative == "less") {
    return(c(
      null = paste0("population mean greater than or equal to", x$null_mean),
      altr = paste0("population mean less than", x$null_mean)
    ))
  }

  # --- all other test hypotheses ---

  if(tt == "two" & x$alternative == "two.sided") {
    return(c(
      null = paste0("population mean are equal"),
      altr = paste0("population means are different")
    ))
  }

  if(tt == "two" & x$alternative == "greater") {
    return(c(
      null = paste0("population mean is equal, or smaller for '", x$group_name[1], "'"),
      altr = paste0("population mean is greater for '", x$group_name[1])
    ))
  }

  if(tt == "two" & x$alternative == "less") {
    return(c(
      null = paste0("population mean is equal, or greater for '", x$group_name[1], "'"),
      altr = paste0("population mean is less for '", x$group_name[1])
    ))
  }

  stop("This should not happen", call. = FALSE)

}



