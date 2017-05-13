
#' Print captcha
#'
#' @param x object
#' @param ... other
#'
#' @export
print.image_captcha <- function(x, ...) {
  print(as.character(x), ...)
}

#' Print captcha
#'
#' @param x object
#' @param ... other
#'
#' @export
print.audio_captcha <- function(x, ...) {
  print(as.character(x), ...)
}
