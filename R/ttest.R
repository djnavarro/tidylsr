
#' Independent samples t-test
#'
#' @param data a data frame or tibble
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quote)
#' @importFrom rlang enquo
#' @importFrom rlang expr
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @export
ttest_independent <- function(data, outcome, group) {

  # construct formula
  lhs <- rlang::enexpr(outcome)
  rhs <- rlang::enexpr(group)
  fml <- as.formula(call("~", lhs, rhs))

  # run the t-test
  ttest <- t.test(fml, data)

  # descriptives
  desc <- data %>%
    dplyr::group_by(!!rhs) %>%
    dplyr::summarise(mean(!!lhs), sd(!!lhs)) %>%
    dplyr::ungroup()

  return(list(desc,ttest))
}

