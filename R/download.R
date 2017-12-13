
#' @title Download captchas from a URL
#'
#' @description This is a generic function to download captchas from any
#' webssite given a captcha's URL. `download_captcha_*()` are aliases for
#' some known captchas of public services in Brazil.
#'
#' @section IP blocking:
#' All downloads have a timeout of three seconds to run.
#' If the website you are accessing blocks IP after multiple calls, consider
#' creating a loop an using [base::Sys.sleep()] to wait for new calls.
#'
#' @param url URL from which to download captcha or the name of a known source
#' (`"tjrs"`, `"tjmg"`, `"tjrj"`, `"trt"` or `"rbf"`)
#' @param n Total number of captchas to download
#' @param path Folder where to save downloaded captchas
#' @param secure Whether or not to use `ssl_verifypeer = TRUE` (see
#' [httr::GET()])
#' @param ext Default image extension if not able to extract it automatically
#'
#' @return A vector with the paths to the downloaded files
#'
#' @export
download_captcha <- function(url, n = 1, path = ".", secure = FALSE, ext = ".jpeg") {

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)

  # Create safe version of download
  safe_download_ <- purrr::possibly(download_captcha_, NA)

  # Iterate over downloads
  out <- c()
  pb <- progress::progress_bar$new(total = n)
  for (i in 1:n) {
    pb$tick()
    out <- c(out, safe_download_(url, path, secure, ext))
  }

  return(out)
}

#' Download captcha from a URL
#'
#' @param url URL from which to download captcha or the name of a known source
#' (`"tjrs"`, `"tjmg"`, `"tjrj"`, `"trt"` or `"rbf"`)
#' @param path Folder where to save downloaded captcha
#' @param secure Whether or not to use `ssl_verifypeer = TRUE` (see
#' [httr::GET()])
#' @param ext Default image extension if not able to extract it automatically
#'
download_captcha_ <- function(url, path, secure, ext) {

  # Replace known captchas
  url <- switch(url,
    "tjrs" = "http://www.tjrs.jus.br/site_php/consulta/human_check/humancheck_showcode.php",
    "tjmg" = "http://www4.tjmg.jus.br/juridico/sf/captcha.svl",
    "tjrj" = "http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha",
    "trt"  = "https://pje.trt4.jus.br/consultaprocessual/seam/resource/captcha",
    "rbf"  = "http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarCaptcha.asp",
    url)

  # Send get request
  httr::handle_reset(url)
  r <- httr::GET(url,
    httr::user_agent("R-decryptr"),
    httr::config(ssl_verifypeer = secure),
    httr::timeout(3))

  # Get file extension
  ct <- r$headers[["content-type"]]
  ext <- ifelse(is.null(ct), ext, stringr::str_c(".", basename(ct)))

  # Save captcha to disk
  file <- tempfile(pattern = "captcha", tmpdir = normalizePath(path), fileext = ext)
  writeBin(r$content, file)

  return(file)
}
