
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
#' @importFrom dplyr %>%
#' @importFrom stats t.test
#' @importFrom stats as.formula
#' @importFrom stats sd
#' @export
ttest_twosample <- function(data, outcome, group) {

  # construct formula
  lhs <- rlang::enexpr(outcome)
  rhs <- rlang::enexpr(group)
  fml <- as.formula(call("~", lhs, rhs))

  # run the t-test
  ttest <- stats::t.test(fml, data)

  # descriptives
  desc <- data %>%
    dplyr::group_by(!!rhs) %>%
    dplyr::summarise(mean(!!lhs), sd(!!lhs)) %>%
    dplyr::ungroup()

  return(list(desc,ttest))
}

