
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
    x$test_type,
    "One sample" = "one",
    "Student" = "two",
    "Welch" = "two",
    "Paired" = "two"
  )

  # --- one sample test hypotheses ---

  if(tt == "one" & x$alternative == "two.sided") {
    return(c(
      null = paste0("population mean equals", x$null_mean),
      altr = paste0("population mean not equal to", x$null_mean)
    ))
  }

  if(tt == "one" & x$alternative == "greater") {
    return(c(
      null = paste0("population mean less than or equal to", x$null_mean),
      altr = paste0("population mean greater than", x$null_mean)
    ))
  }

  if(tt == "one" & x$alternative == "less") {
    return(c(
      null = paste0("population mean greater than or equal to", x$null_mean),
      altr = paste0("population mean less than", x$null_mean)
    ))
  }

  # --- all other test hypotheses ---

  if(tt == "two" & x$alternative == "two.sided") {
    return(c(
      null = paste0("population mean are equal"),
      altr = paste0("population means are different")
    ))
  }

  if(tt == "two" & x$alternative == "greater") {
    return(c(
      null = paste0("population mean is equal, or smaller for '", x$group_name[1], "'"),
      altr = paste0("population mean is greater for '", x$group_name[1])
    ))
  }

  if(tt == "two" & x$alternative == "less") {
    return(c(
      null = paste0("population mean is equal, or greater for '", x$group_name[1], "'"),
      altr = paste0("population mean is less for '", x$group_name[1])
    ))
  }

  stop("This should not happen", call. = FALSE)

}
