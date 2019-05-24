
#' One sample t-test
#'
#' @param data a data frame or tibble
#' @param outcome the outcome variable (quoted)
#' @param null_mean the fixed mean to test against (numeric)
#' @param test_greater alternative hypothesis that the mean exceeds 'null_mean' (TRUE), is less than 'null_mean' (FALSE) or two-sided (NULL, the default)
#' @param ... other arguments to be passed to t.test
#' @export
ttest_onesample <- function(data, outcome = NULL, null_mean = NULL,
                            test_greater = NULL, ...) {

  # extract outcome variable as character string
  outcome <- as.character(rlang::enexpr(outcome))

  # handle missing data
  data <- ttest_handle_missing(data, outcome)

  # extract the sample
  x <- data[[outcome]]

  # specify the alternative
  alt <- get_direction_one(test_greater)

  # run the t-test
  ttest <- stats::t.test(x=x, mu=null_mean, alternative = alt, ...)


  # construct tibble with variable names
  variables <- tibble::tibble(
    outcome = outcome,
    group = NA_character_,
    id = NA_character_,
    sample1 = NA_character_,
    sample2 = NA_character_,
    null_mean = null_mean
  )

  # construct tibble with test/confidence results
  test <- tibble::tibble(
    type = "one_sample",
    hypotheses = alt,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    ci_lower = strip(ttest$conf.int)[1],
    ci_upper = strip(ttest$conf.int)[2],
    ci_level = attr(ttest$conf.int, "conf.level")
  )

  # construct tibble with descriptive statistics
  descriptives <- tibble::tibble(
    sample = outcome,
    mean = mean(x),
    sd = stats::sd(x)
  )

  # format the output
  out <- new_lsr_ttest(
    variables = variables,
    test = test,
    descriptives = descriptives
  )

  return(out)
}



#' Two sample t-test
#'
#' @param data a data frame or tibble
#' @param formula the model formula (i.e., outcome ~ group)
#' @param outcome the outcome variable (quoted)
#' @param group the grouping variable (quoted)
#' @param test_greater group proposed to be larger under the alternative (character), or NULL (the default, for a two-sided test)
#' @param equal_variances should the test assume equality of variance? (default = FALSE)
#' @param ... other arguments to be passed to t.test
#' @export
ttest_twosample <- function(data, formula = NULL, outcome = NULL, group = NULL,
                            test_greater = NULL, equal_variances = FALSE, ...) {

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

  # handle missing data
  data <- ttest_handle_missing(data, c(outcome, group))

  # extract group names and samples
  grp_names <- get_group_names(data[[group]])
  x <- data[[outcome]][data[[group]] == grp_names[1]]
  y <- data[[outcome]][data[[group]] == grp_names[2]]

  # specify the alternative hypothesis
  alt <- get_direction_two(test_greater, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = alt,
                         var.equal = equal_variances, ...)


  # construct tibble with variable names
  variables <- tibble::tibble(
    outcome = outcome,
    group = group,
    id = NA_character_,
    sample1 = grp_names[1],
    sample2 = grp_names[2],
    null_mean = NA_real_
  )

  # construct tibble with test/confidence results
  test <- tibble::tibble(
    type = ifelse(equal_variances, "student", "welch"),
    hypotheses = alt,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    ci_lower = strip(ttest$conf.int)[1],
    ci_upper = strip(ttest$conf.int)[2],
    ci_level = attr(ttest$conf.int, "conf.level")
  )

  # construct tibble with descriptive statistics
  descriptives <- tibble::tibble(
    sample = grp_names,
    mean = c(mean(x), mean(y)),
    sd = c(stats::sd(x), stats::sd(y))
  )

  # format the output
  out <- new_lsr_ttest(
    variables = variables,
    test = test,
    descriptives = descriptives
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
#' @param test_greater group proposed to be larger under the alternative (character), or NULL (the default, for a two-sided test)
#' @param ... other arguments to be passed to t.test
#' @importFrom dplyr %>%
#' @export
ttest_paired <- function(data, formula = NULL, outcome = NULL, group = NULL,
                         id = NULL, test_greater = NULL, ...) {

  # if there is no formula, use the named arguments
  if(is.null(formula)) {
    outcome <- rlang::enexpr(outcome)
    group <- rlang::enexpr(group)
    id <- rlang::enexpr(id)

  # but use the formula version by default
  } else {
    tmp <- unpack_paired_formula(formula)
    outcome <- tmp$outcome
    group <- tmp$group
    id <- tmp$id
  }


  # create a wide form version of the data
  wide_data <- data %>%
    dplyr::select(!!id, !!group, !!outcome) %>%
    tidyr::spread(key = !!group, value = !!outcome)

  # outcome and group can just be strings
  outcome <- as.character(outcome)
  group <- as.character(group)
  id <- as.character(id)

  # extract group names
  grp_names <- get_group_names(data[[group]])

  # handle missing data
  wide_data <- ttest_handle_missing(wide_data, grp_names)

  # extract the paired samples
  x <- wide_data[[grp_names[1]]]
  y <- wide_data[[grp_names[2]]]

  # specify the alternative hypothesis
  alt <- get_direction_two(test_greater, grp_names)

  # run the t-test
  ttest <- stats::t.test(x=x, y=y, alternative = alt, paired = TRUE, ...)

  # construct tibble with variable names
  variables <- tibble::tibble(
    outcome = outcome,
    group = group,
    id = id,
    sample1 = grp_names[1],
    sample2 = grp_names[2],
    null_mean = NA_real_
  )

  # construct tibble with test/confidence results
  test <- tibble::tibble(
    type = "paired",
    hypotheses = alt,
    t = strip(ttest$statistic),
    df = strip(ttest$parameter),
    p = strip(ttest$p.value),
    ci_lower = strip(ttest$conf.int)[1],
    ci_upper = strip(ttest$conf.int)[2],
    ci_level = attr(ttest$conf.int, "conf.level")
  )

  # construct tibble with descriptive statistics
  descriptives <- tibble::tibble(
    sample = c(grp_names, "diff."),
    mean = c(mean(x), mean(y), mean(x-y)),
    sd = c(stats::sd(x), stats::sd(y), stats::sd(x-y))
  )

  # format the output
  out <- new_lsr_ttest(
    variables = variables,
    test = test,
    descriptives = descriptives
  )

  return(out)
}





