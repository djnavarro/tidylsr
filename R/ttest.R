
#' Two samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param one_sided_null an expression specifying a one-sided null hypothesis (quoted) or FALSE (default, indicates two sided)
#' @param equal_variances should the test assume equality of variance? (default = FALSE)
#' @param ... other arguments to be passed to t.test
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom stats t.test
#' @importFrom stats as.formula
#' @importFrom stats sd
#' @export
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL,
                            one_sided_null = FALSE, equal_variances = FALSE, ...) {

  # construct formula, outcome and group ------------------------------------
  if(is.null(formula)) {
    # if the user does not specify a formula look for
    # outcome and group arguments to construct it
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
    formula <- as.formula(call("~", outcome, group))

  } else {
    # if the user specifies a formula, extract the
    # outcome and group variables from it
    outcome <- formula[[2]]
    group <- formula[[3]]
  }


  # calculate relevant descriptives -----------------------------------------
  desc <- data %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarise(m = mean(!!outcome), s = sd(!!outcome)) %>%
    dplyr::ungroup()
  grp_names <- dplyr::pull(desc, !!group)


  # specify null hypothesis -------------------------------------------------
  if(is.logical(one_sided_null) && one_sided_null == FALSE) {
    hyp <- c(null = "equal means", alternative = "different means")
    alt <- "two.sided"
  } else {
    tmp <- rlang::enexpr(one_sided_null)
    hyp <- ttest_check_null(tmp, grp_names)
    alt <- ttest_convert_hyp(hyp, grp_names)
  }



  # run the t-test ----------------------------------------------------------
  ttest <- stats::t.test(formula, data, alternative = alt,
                         var.equal = equal_variances, ...)


  # format the output -------------------------------------------------------
  out <- list(
    variables = c(outcome = as.character(outcome),
                  group = as.character(group),
                  id = NA),
    test = c(t = strip(ttest$statistic),
             df = strip(ttest$parameter),
             p = strip(ttest$p.value)),
    conf_int = strip(ttest$conf.int),
    conf_lvl = attr(ttest$conf.int, "conf.level"),
    group_mean = desc$m,
    group_sd = desc$s,
    group_name = grp_names,
    hypotheses = hyp,
    test_type = ifelse(equal_variances, "Student", "Welch"),
    null_mean = NULL,
    effect_size = NULL
  )

  return(out)
}



ttest_check_null <- function(h0, groups) {

  if(is.logical(h0) && h0 == FALSE) {
    hyp <- c(null = "equal means", alternative = "different means")
  } else {
    hyp <- c(null = "equal means", alternative = "different means")
  }
  return(hyp)

}

ttest_convert_hyp <- function(hyps, groups) {

  return("two.sided")

}
