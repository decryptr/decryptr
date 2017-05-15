#' Download captcha from any site
#'
#' Generic download function
#'
#' @param url url to download
#' @param dest destination
#' @param secure use ssl verifypeer as FALSE
#' @param type image extension. Normally 'jpeg' or 'png'
#'
#' @export
download <- function(url, dest = NULL, secure = FALSE, type = 'jpeg') {
  # Build URL for request
  # If dest is null, create temporary file
  httr::handle_reset(url)
  .type <- paste0('.', type)
  if (is.null(dest)) {
    dest <- tempfile(pattern = 'captcha', fileext = .type)
  } else {
    dest <- tempfile(pattern = 'captcha', tmpdir = dest, fileext = .type)
  }
  # Send get request
  httr::GET(url, httr::config(ssl_verifypeer = secure),
            httr::write_disk(dest, overwrite = TRUE))
  read_captcha(dest)
}

download_tjrs <- function(dest = NULL) {
  # Build URL for request
  url <- paste0(
    'http://www.tjrs.jus.br/site_php/consulta',
    '/human_check/humancheck_showcode.php')
  download(url, dest)
}


download_tjmg <- function(dest = NULL) {
  # Build URL for request
  url <- 'http://www4.tjmg.jus.br/juridico/sf/captcha.svl'
  download(url, dest)
}

download_trt <- function(dest = NULL) {
  # Build URL for request
  url <- 'https://pje.trt3.jus.br/consultaprocessual/seam/resource/captcha'
  # Error in curl::curl_fetch_disk(url, x$path, handle = handle) :
  # Couldn't connect to server
  download(url, dest)
}

download_receita <- function(dest = NULL) {
  # Build URL for request
  url <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  download(url, dest)
}

download_saj <- function(dest = NULL) {
  # Build URL for request
  url <- ('https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do')
  download(url, dest)
}


download_tjrj <- function(dest = NULL) {
  # Build URL for request
  url <- ('http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha')
  # If dest is null, create temporary file
  download(url, dest, secure = FALSE)
}



