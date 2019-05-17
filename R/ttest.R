
#' Twor samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
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
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL) {

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

  # descriptives
  desc <- data %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarise(m = mean(!!outcome), s = sd(!!outcome)) %>%
    dplyr::ungroup()

  # run the t-test
  ttest <- stats::t.test(formula, data)


  out <- list(
    t = ttest$statistic,
    df = ttest$parameter,
    p = ttest$p.value,
    ci = strip(ttest$conf.int),
    ci_lvl = attr(ttest$conf.int, "conf.level"),
    grp_mean = desc$m,
    grp_sd = desc$s,
    grp_names = dplyr::pull(desc, !!group),
    out = outcome,
    grp = group
    #group.names = gp.names,
    #id = NULL,
    #mu = NULL,
    #alternative = alternative,
    #method = ifelse( var.equal,
    #                 yes = "Student's independent samples t-test",
    #                 no = "Welch's independent samples t-test" ),
    #effect.size = d
  )



  return(out)
}

