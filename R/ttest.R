
#' Two samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param alternative an expression specifying the null hypothesis (quoted) or FALSE (default, indicates two sided)
#' @param equal_variances should the test assume equality of variance? (default = FALSE)
#' @param ... other arguments to be passed to t.test
#' @export
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL,
                            alternative = NULL, equal_variances = FALSE, ...) {

  # alternative, outcome and group as expressions
  alternative <- rlang::enexpr(alternative)
  if(is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
  } else {
    outcome <- formula[[2]]
    group <- formula[[3]]
  }

  # outcome and group can just be strings
  outcome <- as.character(outcome)
  group <- as.character(group)

  # extract the group names and the two samples
  grp_names <- unique(data[[group]])
  x <- data[[outcome]][data[[group]] == grp_names[1]]
  y <- data[[outcome]][data[[group]] == grp_names[2]]

  # specify hypothesis
  hyp <- ttest_build_hyp(alternative, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = hyp$base,
                         var.equal = equal_variances, ...)

  # don't store group names as a factor
  if(is.factor(grp_names)) grp_names <- as.character(grp_names)

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
    hypotheses = hyp$tidy,
    test_type = ifelse(equal_variances, "Student", "Welch")
  )

  return(out)
}



#' Paired samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param id the id variable (qouoted)
#' @param alternative an expression specifying the null hypothesis (quoted) or FALSE (default, indicates two sided)
#' @param ... other arguments to be passed to t.test
#' @importFrom dplyr %>%
#' @export
ttest_paired <- function(data, formula = NULL, outcome = NULL, group = NULL,
                         id = NULL, alternative = NULL,  ...) {

  # alternative, outcome, group, id as expressions
  alternative <- rlang::enexpr(alternative)
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

  # extract the group names and the two samples
  grp_names <- unique(data[[group]])

  # create a wide form version of the data
  wide_data <- data %>%
    dplyr::select(!!id, !!group, !!outcome) %>%
    tidyr::spread(key = !!group, value = !!outcome)

  # extract the paired samples
  x <- wide_data[[grp_names[1]]]
  y <- wide_data[[grp_names[2]]]

  # specify hypothesis
  #hyp <- ttest_build_hyp(alternative, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = "two.sided", paired = TRUE, ...)

  # don't store group names as a factor
  if(is.factor(grp_names)) grp_names <- as.character(grp_names)

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
    #hypotheses = hyp$tidy,
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

  # check the hypothesis
  if(alternative == "two.sided") {
    hyp <- c(
      null = paste0("population mean of `", outcome, "` equals ", null_mean),
      alternative = paste0("population mean of `", outcome, "` differs from ", null_mean)
    )
  } else if(alternative == "greater") {
    hyp <- c(
      null = paste0("population mean of `", outcome, "` is less than or equal to ", null_mean),
      alternative = paste0("population mean of `", outcome, "` is greater than ", null_mean)
    )
  } else if(alternative == "less") {
    hyp <- c(
      null = paste0("population mean of `", outcome, "` is greater than or equal to ", null_mean),
      alternative = paste0("population mean of `", outcome, "` is less than ", null_mean)
    )
  } else {
    stop('`alternative` must be "two.sided", "greater" or "less"', call. = FALSE)
  }


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
    hypotheses = hyp,
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
                          hypotheses = NULL, test_type = NULL, null_mean = NULL,
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
      hypotheses = hypotheses,
      test_type = test_type,
      null_mean = null_mean,
      effect_size = effect_size
    ),
    class = "lsr_ttest"
  )

}


# construct a tidy version of the null and alternative hypothesis, as well as
# the character string used by t.test in base R.
ttest_build_hyp <- function(alt, grp) {

  alt <- rlang::expr(!!alt)

  # two sided test
  if(is.null(alt)) {
    return(list(
      tidy <- c(null = "equal means", alternative = "different means"),
      base <- "two.sided"
    ))
  }

  # one sided test should be an expression of length 3
  if(length(alt) != 3) { stop("invalid expression") }

  # parse input
  dir <- as.character(alt[[1]])
  if(dir == ">") {
    x <- as.character(alt[[2]])
    y <- as.character(alt[[3]])
  } else if(dir == "<") {
    x <- as.character(alt[[2]])
    y <- as.character(alt[[3]])
  } else {
    stop("one-sided `alternative` must specify directon using `<` or `>`")
  }

  # string describing the tidy hypothesis
  tidy = c(null = paste0(x, " <= ", y),
           alternative = paste0(x, " > ", y))

  # if user has specified groups in the expected order
  # (x maps to first, y maps to second) return "greater"
  if(x == grp[1] & y == grp[2]) {
    return(list(tidy = tidy, base = "greater"))
  }

  # if user spec is reverse, return "less"
  if(x == grp[2] & y == grp[1]) {
    return(list(tidy = tidy, base = "less"))
  }

  # it it does not match, throw error
  stop("group names in `alternative` must match `data`")

}




