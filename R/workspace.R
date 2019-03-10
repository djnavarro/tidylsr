#' Prints the contents of the global environment
#'
#' @return Something
#' @examples
#' workspace()
#' @importFrom tibble tibble
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
    length(eval(as.symbol(x)))
  }


  eval_there <- function(fn) {
    eval(
      expr = sapply(objects(envir = call_env), fn),
      envir = call_env
    )
  }

  wspace <- tibble::tibble(name = objects(envir = call_env))
  wspace$class <- eval_there(get_classes)
  wspace$length <- eval_there(get_lengths)

  return(wspace)
}
