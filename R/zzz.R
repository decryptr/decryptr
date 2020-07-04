globalVariables(c(
  "arq", "n", ".", "g", "r", "b", "y", "x", "group", "letras",
  "letras", "rowname", "v", "letra", "xy", "cor", "key", "value",
  "model.matrix", "predict", "y_train", "model", "id_train"
))

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

.onLoad <- function(libname, pkgname) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    warning("Initialize TF with 'reticulate::py_available(TRUE)'", call. = FALSE)
    return(FALSE)
  }

  invisible(reticulate::py_available(TRUE))
}
