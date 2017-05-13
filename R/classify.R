
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
classify.image_captcha <- function(x, dest, answer = NULL, ...) {
  suppressWarnings(dir.create(dest))
  if(is.null(answer)) { graphics::plot(x) }
  if(is.null(answer)) { answer <- readline(prompt="Answer: ") }
  out <- sprintf('%s/%s_%s.jpeg', dest, tools::file_path_sans_ext(basename(x)), answer)
  file.copy(x, out)
  class(out) <- c("image_captcha", "tjrs")
  invisible(out)
}
