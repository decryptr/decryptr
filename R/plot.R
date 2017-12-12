#' Read PNG or JPG
#'
#' @param x object
#' @param ... other
load_image <- function(x, ...) {
  # Get extention
  ext <- tolower(tools::file_ext(basename(x)))
  if (ext %in% c("jpeg", "jpg")) {
    img <- jpeg::readJPEG(x)
  } else if (ext == "png") {
    img <- png::readPNG(x)
  }
  return(img)
}

#' Plot a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param y -
#' @param ... Other arguments passed on to [graphics::plot()]
#'
#' @export
plot.captcha <- function(x, y, ...) {
  img <- load_image(x)
  op <- graphics::par(mar = rep(0, 4))
  graphics::plot(grDevices::as.raster(img), ...)
  graphics::par(op)
}
