
#' Plot a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param ... Other arguments passed on to [graphics::plot()]
#'
#' @export
print.captcha <- function(x, ...) {
  plot.captcha(x)
}

print.model <- function(x, ...) {
  model$model
}
