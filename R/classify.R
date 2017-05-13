#' Classify captcha
#'
#' @param x object
#' @param ... other
#'
#' @export
classify <- function(x, ...) {
  UseMethod("classify")
}

#' Classify captcha
#'
#' @param x object
#' @param dest destination
#' @param answer answer
#' @param ... other
#'
#' @export
classify.captcha <- function(x, dest, answer = NULL, ...) {
  suppressWarnings(dir.create(dest))
  if(is.null(answer)) { graphics::plot(x) }
  if(is.null(answer)) { answer <- readline(prompt="Answer: ") }
  nm <- tools::file_path_sans_ext(basename(x))
  ext <- tools::file_ext(x)
  out <- sprintf('%s/%s_%s.%s', dest, nm, answer, ext)
  file.copy(x, out)
  class(out) <- c("captcha", "captcha_image", "tjrs")
  invisible(out)
}
