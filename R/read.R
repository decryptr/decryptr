
read <- function(path, ...) {
  UseMethod("read")
}

read.default <- function(path, ...) {
  stop("Undefined type")
}

#' Read TRJS
#'
#' @export
read.captcha_tjrs <- function(x, ...) {
  magick::image_read()
}
