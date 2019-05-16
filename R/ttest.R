
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
#' @importFrom stats t.test
#' @importFrom stats as.formula
#' @importFrom stats sd
#' @export
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL) {

  # construct formula if needed
  if(!is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
    formula <- as.formula(call("~", outcome, group))
  }

  # run the t-test
  ttest <- stats::t.test(formula, data)

  # descriptives
  desc <- data %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarise(mean(!!outcome), sd(!!outcome)) %>%
    dplyr::ungroup()

  result <- list(desc, ttest)

  return(result)
}

