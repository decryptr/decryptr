#' Donload captcha from TRJS
#'
#' @export
download_tjrs <- function(path = NULL) {
  url <- paste0(
    'http://www.tjrs.jus.br/site_php/consulta',
    '/human_check/humancheck_showcode.php'
  )
  if (is.null(path)) {
    path <- tempfile(pattern = 'captcha', fileext = '.jpeg')
  }
  httr::GET(url, httr::write_disk(path, overwrite = TRUE))

  class(path) <- "tjrs"
  return(path)
}
