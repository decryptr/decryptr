download <- function(url, dest, n, secure = FALSE, type = 'jpeg') {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  safe_download_one <- purrr::possibly(download_one, 'error')
  p <- progress::progress_bar$new()
  result <- purrr::map_chr(seq_len(n), ~{
    result <- safe_download_one(url, dest, secure, type)
    p$tick()
    result
  })
  invisible(result)
}

download_one <- function(url, dest, secure, type) {
  httr::handle_reset(url)
  .type <- paste0('.', type)
  dest <- tempfile(pattern = 'captcha', tmpdir = dest, fileext = .type)
  # Send get request
  httr::GET(url, httr::config(ssl_verifypeer = secure),
            httr::write_disk(dest, overwrite = TRUE))
  dest
}

# #' Download captcha from any site
# #'
# #' Generic download function
# #'
# #' @param url url to download
# #' @param dest destination
# #' @param secure use ssl verifypeer as FALSE
# #' @param type image extension. Normally 'jpeg' or 'png'
# #'
# #' @export

download_tjrs <- function(dest = NULL, n = 1) {
  url <- paste0(
    'http://www.tjrs.jus.br/site_php/consulta',
    '/human_check/humancheck_showcode.php'
  )
  download(url, dest, n = n)
}

download_tjmg <- function(dest = NULL, n = 1) {
  url <- 'http://www4.tjmg.jus.br/juridico/sf/captcha.svl'
  download(url, dest, n = n)
}

download_trt <- function(dest = NULL, n = 1) {
  url <- 'https://pje.trt3.jus.br/consultaprocessual/seam/resource/captcha'
  download(url, dest, n = n)
}

download_receita <- function(dest = NULL, n = 1) {
  url <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/cnpjreva_solicitacao2.asp'
  download(url, dest, n = n)
}

download_saj <- function(dest = NULL, n = 1) {
  url <- ('https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do')
  download(url, dest, n = n)
}

download_tjrj <- function(dest = NULL, n = 1) {
  url <- ('http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha')
  download(url, dest, n = n, secure = FALSE)
}
