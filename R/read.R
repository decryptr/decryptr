
read <- function(path, ...) {
  UseMethod("read")
}

read.default <- function(path, ...) {
  stop("Undefined type")
}

#' Read TRJS
#'
#' @export
read.tjrs_download <- function(path, ...) {
  img <- jpeg::readJPEG(path)
  img_dim <- dim(img)
  img_df <- data.frame(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')

  out <- tibble::as_data_frame(d)
  class(out) <- "trjs_tibble"
  return(out)
}
