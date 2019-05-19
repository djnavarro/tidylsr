
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
    group_mean = c(mean(x), mean(y)),
    group_sd = c(stats::sd(x), stats::sd(y)),
    group_name = grp_names,
    hypotheses = hyp$tidy,
    test_type = ifelse(equal_variances, "Student", "Welch")
  )

  return(out)
}


# Constructor function for the lsr_ttest class. At the moment
# it is just ridiculous duplication but I suspect I'll want to
# use it to check inputs in some cases?
new_lsr_ttest <- function(outcome = NULL, group = NULL, id = NULL, t = NULL,
                          df = NULL, p = NULL, conf_int = NULL, conf_lvl = NULL,
                          group_mean = NULL, group_sd = NULL, group_name = NULL,
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
      group_mean = group_mean,
      group_sd = group_sd,
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




