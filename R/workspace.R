#' Prints the contents of the global environment
#'
#' @return Something
#' @examples
#' workspace()
#' @importFrom tibble tibble
#' @importFrom vctrs vec_size
#' @export
workspace <- function() {

  # the calling environment
  call_env <- parent.frame()

  # takes a character vector of variable names
  # and returns the (first) class of each variable
  get_classes <- function(x) {
    class(eval(as.symbol(x)))[1]
  }

  # takes a character vector of variable names
  # and returns the length of each variable
  get_lengths <- function(x) {
    #length(eval(as.symbol(x)))
    len <- try(
      expr = vctrs::vec_size(eval(as.symbol(x))),
      silent=TRUE)
    if(is(len, "try-error")) len <- NA
    return(len)
  }

  # evaluate a function on all objects in the
  # calling environment
  eval_there <- function(fn) {
    eval(
      expr = sapply(objects(envir = call_env), fn),
      envir = call_env
    )
  }

  # return the tibble
  wspace <- tibble::tibble(name = objects(envir = call_env))
  wspace$class <- eval_there(get_classes)
  wspace$length <- eval_there(get_lengths)

  return(wspace)
}
