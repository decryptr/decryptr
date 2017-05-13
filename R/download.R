#' Download captcha from TJRS
#'
#' @param dest destination
#'
#' @export

download <- function(url, dest = NULL, secure = TRUE) {
  # Build URL for request
  # If dest is null, create temporary file
  if (is.null(dest)) {
    dest <- tempfile(pattern = 'captcha', fileext = '.jpeg')
  }   else {
    dest <- tempfile(pattern = 'captcha', tmpdir = dest, fileext = '.jpeg')
  }
  # Send get request

  httr::GET(url, httr::write_disk(dest, overwrite = TRUE),
            httr::config(ssl_verifypeer = secure))
  read_captcha(dest)

}

download_tjrs <- function(dest = NULL) {
  # Build URL for request
  url <- paste0(
    'http://www.tjrs.jus.br/site_php/consulta',
    '/human_check/humancheck_showcode.php')
  download(url)
}


download_tjmg <- function(dest = NULL) {
  # Build URL for request
  url <- 'http://www4.tjmg.jus.br/juridico/sf/captcha.svl'
  download(url)
}

download_trt <- function(dest = NULL) {
  # Build URL for request
  url <- 'https://pje.trt3.jus.br/consultaprocessual/seam/resource/captcha'
  # Error in curl::curl_fetch_disk(url, x$path, handle = handle) :
  # Couldn't connect to server
  download(url)
}

download_receita <- function(dest = NULL) {
  # Build URL for request
  url <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  download(url)
}

download_saj <- function(dest = NULL) {
  # Build URL for request
  url <- ('https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do')
  download(url)
}


download_tjrj <- function(dest = NULL) {
  # Build URL for request
  url <- ('http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha')
  # If dest is null, create temporary file
  download(url, secure = FALSE)
}



