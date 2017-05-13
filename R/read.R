#' Read TJRS
#'
#' @param path path
#'
#' @export
read_tjrs <- function(path) {
  # Get extention
  ext <- tolower(tools::file_ext(basename(path)))
  # Return path
  if (all(ext %in% c("jpeg", "jpg"))) {
    message("File was located and will be brought to memory when used")
    class(path) <- c("captcha", "image_captcha", "tjrs")
    return(path)
  }
  # Error
  stop("File isn't of type TJRS")
}
