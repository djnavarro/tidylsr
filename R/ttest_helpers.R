
# convert the paired model formula into a list of names
unpack_paired_formula <- function(fml) {
  out <- fml[[2]]
  rhs <- fml[[3]]
  var <- c(rhs[[2]], rhs[[3]])
  ind <- grep("^\\(.*\\)$", var)
  grp <- var[[3 - ind]]
  ids <- var[[ind]][[2]] # [[1]] is "(", [[2]] is id
  return(list(
    outcome = out,
    group = grp,
    id = ids
  ))
}

# specify the test direction
get_direction_two <- function(grt, grp_names) {
  if(is.null(grt)) return("two.sided")
  if(grt == grp_names[1]) return("greater")
  if(grt == grp_names[2]) return("less")
  stop("`test_greater` must be NULL or a value indicating a group",
       call. = FALSE)
}

# specify the test direction for a one sample test
get_direction_one <- function(grt) {
  if(is.null(grt)) return("two.sided")
  if(grt == TRUE) return("greater")
  if(grt == FALSE) return("less")
  stop("`test_greater` must be NULL, TRUE or FALSE", call. = FALSE)
}

# extract group names (and don't return a factor)
get_group_names <- function(grp) {
  grp_names <- unique(grp)
  if(is.factor(grp_names)) grp_names <- as.character(grp_names)
  return(grp_names)
}

# inputs an lsr_ttest object and outputs a character vector
# specifying the hypothesis in a verbose, human readable form
get_verbose_hypotheses <- function(x) {

  tt <- switch(
    x$test$type,
    "one_sample" = "one",
    "student" = "two",
    "welch" = "two",
    "paired" = "two"
  )

  sm <- x$variables$sample1
  nm <- x$variables$null_mean
  xa <- x$test$hypotheses

  # --- one sample test hypotheses ---

  if(tt == "one" & xa == "two.sided") {
    return(c(
      null = paste0("population mean equals", nm),
      altr = paste0("population mean not equal to", nm)
    ))
  }

  if(tt == "one" & xa == "greater") {
    return(c(
      null = paste0("population mean less than or equal to", nm),
      altr = paste0("population mean greater than", nm)
    ))
  }

  if(tt == "one" & xa == "less") {
    return(c(
      null = paste0("population mean greater than or equal to", nm),
      altr = paste0("population mean less than", nm)
    ))
  }

  # --- all other test hypotheses ---

  if(tt == "two" & xa == "two.sided") {
    return(c(
      null = paste0("population mean are equal"),
      altr = paste0("population means are different")
    ))
  }

  if(tt == "two" & xa == "greater") {
    return(c(
      null = paste0("population mean is equal, or smaller for '", sm, "'"),
      altr = paste0("population mean is greater for '", sm)
    ))
  }

  if(tt == "two" & xa == "less") {
    return(c(
      null = paste0("population mean is equal, or greater for '", sm, "'"),
      altr = paste0("population mean is less for '", sm)
    ))
  }

  stop("This should not happen", call. = FALSE)

}
