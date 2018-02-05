
#' Print information about a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param ... -
#'
#' @export
print.captcha <- function(x, ...) {
  if (!is.null(purrr::pluck(x, "x"))) {
    cat("A captcha located at:\n\"", as.character(attr(x, "file")), "\"", sep = "")
  } else {
    cat("A list of", length(x), "captchas located at:\n")
    for (i in seq_along(x)) {
      idx <- stringr::str_pad(i, stringr::str_length(length(x)), side = "left", pad = "0")
      cat(idx, ". \"", as.character(attr(x[[i]], "file")), "\"\n", sep = "")
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

  # Stop if list
  if (is.null(purrr::pluck(x, "x"))) {
    stop("Can't plot a list of captchas, use `[[`")
  }

  # Plot
  img <- load_image(as.character(attr(x, "file")))
  op <- graphics::par(mar = rep(0, 4))
  graphics::plot(grDevices::as.raster(img), ...)
  graphics::par(op)

  # Return raster silently
  invisible(grDevices::as.raster(img))
}
