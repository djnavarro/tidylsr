strip <- function(x) {
  attributes(x) <- NULL
  x
}