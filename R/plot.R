#' Read PNG or JPG
#'
#' @param x object
#' @param ... other
#'
#' @export
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

#' Plot captcha
#'
#' @param x object
#' @param y -
#' @param ... other
#'
#' @export
plot.captcha <- function(x, y, ...) {
  img <- load_image(x)
  op <- par(mar = rep(0, 4))
  graphics::plot(grDevices::as.raster(img))
  par(op)
}

