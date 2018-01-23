
#' Print information about a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param ... -
#'
#' @export
print.captcha <- function(x, ...) {
  if (length(x) == 1) {
    cat("A captcha located at:\n", as.character(attr(x[[1]], "file")), sep = "")
  } else {
    cat("A list of ", length(x), " captchas located at:\n")
    for (i in seq_along(x)) {
      cat(i, ". ", as.character(attr(x[[i]], "file")), "\n", sep = "")
    }
  }
}

#' Print information about a model
#'
#' @param x Model object loaded with [load_model()]
#' @param ... -
#'
#' @export
print.model <- function(x, ...) {
  print(model$model)
}

#' Plot a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param y -
#' @param ... Other arguments passed on to [graphics::plot()]
#'
#' @export
plot.captcha <- function(x, y, ...) {
  if (length(x) == 1) { x <- x[[1]] }
  img <- load_image(as.character(attr(x, "file")))
  op <- graphics::par(mar = rep(0, 4))
  graphics::plot(grDevices::as.raster(img), ...)
  graphics::par(op)
}
