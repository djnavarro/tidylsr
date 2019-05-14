#' Displays the contents of the workspace
#'
#' @return Something
#' @examples
#' seeker <- 1
#' lover <- "keeper"
#' show_workspace()
#' @importFrom tibble tibble
#' @importFrom vctrs vec_size
#' @importFrom methods is
#' @export
show_workspace <- function() {

  # the calling environment
  call_env <- parent.frame()

  # evaluate in the calling environment
  eval_there <- function(expr) {
    eval(expr, call_env)
  }

  # takes a symbol and returns the (first) class
  # of the corresponding variable in the calling
  # environment
  get_class <- function(x) {
    class(eval_there(x))[1]
  }

  # takes a character vector of variable names
  # and returns information about the size
  get_size <- function(x) {

    # try to calculate dimension
    size <- try(expr = dim(eval_there(x)), silent = TRUE)

    # rectangular objects:
    if(!is(size, "NULL") & length(size) == 2) {
      msg <- paste0("rectangular: ", size[1], " by ",  size[2])
      return(msg)
    }

    # other objects with a dimension
    if(!is(size, "NULL") & length(size) != 2) {
      msg <- paste(length(size), "dimensional object")
      return(msg)
    }

    # if it has a vector length, return that
    #if(is(eval_there(x), "vector")) {
      size <- try(expr = vctrs::vec_size(eval_there(x)), silent = TRUE)

      if(!is(size, "try-error")) {
        msg <- paste("length:", size)
        return(msg)
      }
    #}

    # if all else fails return an empty string
    return(NA)
  }

  # make a vector of variable names, and the
  # corresponding list of symbols
  var_names <- objects(envir = call_env)
  var_symbols <- lapply(var_names, as.symbol)

  # construct the tibble
  wspace <- tibble::tibble(
    variable = var_names,
    class = sapply(var_symbols, get_class),
    size = sapply(var_symbols, get_size)
  )

  return(wspace)
}
