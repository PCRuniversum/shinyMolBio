# Given a vector or list, drop all the NULL items in it
.dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}