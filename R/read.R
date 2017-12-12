#' @title Read captcha file
#'
#' @description Given the path to a file, reads that file and converts
#' it into a `captcha` object that can be used for classification or
#' decryption.
#'
#' @param path One or more paths to the images of captchas
#'
#' @export
read_captcha <- function(path) {

  # Get extention
  ext <- tolower(tools::file_ext(basename(path)))

  # Return path
  if (all(ext %in% c("jpeg", "jpg", "png"))) {
    class(path) <- c("captcha")
    return(prepare(path))
  }

  # Error
  stop("All files must have extensions 'jpeg', 'jpg' or 'png'.")
}
