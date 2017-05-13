#' Read captcha image
#'
#' @param path path
#'
#' @export
read_captcha <- function(path, ...) {
  # Get extention
  ext <- tolower(tools::file_ext(basename(path)))
  # Return path
  if (all(ext %in% c("jpeg", "jpg", "png"))) {
    message("File(s) was located and will be brought to memory when used")
    class(path) <- c("captcha")
    return(path)
  }
  # Error
  stop("All files must have extensions 'jpeg', 'jpg' or 'png'.")
}

