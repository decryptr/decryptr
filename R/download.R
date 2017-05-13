#' Download captcha from TJRS
#'
#' @param dest destination
#'
#' @export
download_tjrs <- function(dest = NULL) {
  # Build URL for request
  url <- paste0(
    'http://www.tjrs.jus.br/site_php/consulta',
    '/human_check/humancheck_showcode.php'
  )
  # If dest is null, create temporary file
  if (is.null(dest)) {
    dest <- tempfile(pattern = 'captcha', fileext = '.jpeg')
  }
  # Send get request
  httr::GET(url, httr::write_disk(dest, overwrite = TRUE))
  # Return destination with appropriate classes
  class(dest) <- c("captcha", "image_captcha", "tjrs")
  return(dest)
}
