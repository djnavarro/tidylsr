
#' Two samples t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param alternative an expression specifying the null hypothesis (quoted) or FALSE (default, indicates two sided)
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
                            alternative = NULL, equal_variances = FALSE, ...) {

  # quote the to-be-quoted arguments
  alternative <- rlang::enexpr(alternative)
  outcome <- rlang::enexpr(outcome)
  group <- rlang::enexpr(group)

  # set up
  mod <- ttest_build_mod(formula, !!outcome, !!group)  # specify model
  desc <- ttest_build_desc(data, mod)                  # descriptives
  grp_names <- dplyr::pull(desc, 1)                    # group names
  hyp <- ttest_build_hyp(alternative, grp_names)       # specify hypotheses

  # run the t-test
  ttest <- stats::t.test(mod$formula, data, alternative = hyp$base,
                         var.equal = equal_variances, ...)

  # format the output
  out <- list(
    var_outcome = as.character(mod$outcome),
    var_group = as.character(mod$group),
    var_id = NA,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    conf_int = strip(ttest$conf.int),
    conf_lvl = attr(ttest$conf.int, "conf.level"),
    group_mean = desc$m,
    group_sd = desc$s,
    group_name = grp_names,
    hypotheses = hyp$tidy,
    test_type = ifelse(equal_variances, "Student", "Welch"),
    null_mean = NULL,
    effect_size = NULL
  )

  return(out)
}



ttest_build_hyp <- function(alt_expr, groups) {

  alt_expr <- rlang::expr(!!alt_expr)

  # two sided test
  if(is.null(alt_expr)) {
    return(list(
      tidy <- c(null = "equal means", alternative = "different means"),
      base <- "two.sided"
    ))
  }

  # one sided test should be an expression of length 3
  if(length(alt_expr) != 3) {
    stop("invalid expression")
  }

  # parse input
  groups <- as.character(groups)
  group1 <- as.character(alt_expr[[2]])
  group2 <- as.character(alt_expr[[3]])
  direction <- as.character(alt_expr[[1]])

  flag <- 0

  # if the data has the groups in the opposite order to the
  # one specified by the user, reverse the labels
  if(group1 == groups[2] && group2 == groups[1]) {
    tmp <- group1
    group1 <- group2
    group2 <- tmp
    flag <- 1-flag

  } else if (group1 != groups[1] || group2 != groups[2]) {
    stop("one-sided hypotheses must specify group names and direction")
  }

  # flip group ordering if need be
  if(direction == "<") {
    tmp <- group1
    group1 <- group2
    group2 <- tmp
    flag <- 1- flag

  } else if(!(direction == ">")) {
    stop("one-sided hypotheses must specify group names and direction")
  }

  return(list(
    tidy = c(null = paste0(group1, " <= ", group2),
             alternative = paste0(group1, " > ", group2)),
    base = ifelse(flag==0, "greater", "less")
  ))
}

ttest_build_mod <- function(formula, outcome, group) {

  # build formula from outcome and group
  if(is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
    formula <- as.formula(call("~", outcome, group))

  # or extract outcome and group from formula
  } else {
    outcome <- formula[[2]]
    group <- formula[[3]]
  }

  # return all three
  return(list(formula = formula, outcome = outcome, group = group))

}

ttest_build_desc <- function(data, mod) {
  data %>%
    dplyr::group_by(!!(mod$group)) %>%
    dplyr::summarise(m = mean(!!(mod$outcome)), s = sd(!!(mod$outcome))) %>%
    dplyr::ungroup()
}


