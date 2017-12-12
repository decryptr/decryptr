#' Download captcha from a site
#'
#' \code{download_captcha} is a generic function to download captchas from any
#' site, given the url captcha. It is possible to download many captchas at
#' onde with the \code{n} option. \code{download_captcha_*} are aliases for
#' some known captchas of public services in Brazil.
#'
#' @param url url to download. Must be a valid url to download the captcha.
#'   One may check whether the url is working or not running
#'   \code{\link[httr]{BROWSE}(url)}.
#' @param n total number of captchas to download. Defaults to one.
#' @param dest captcha file destination. Defaults to current directory.
#' @param secure use option \code{ssl_verifypeer = TRUE}? Defaults to \code{FALSE}.
#' @param .default image extension if not extracted automatically from
#'   \code{httr::GET(url, ...)$headers[["content-type"]]}.
#'   Normally "jpeg" or "png". Defaults to "jpeg".
#'
#' All downloads have a timeout of three seconds to run. If the site you are
#' accessing blocks IP after multiple calls, consider creating a loop an using
#' \code{\link[base]{Sys.sleep}} to wait new calls.
#'
#' @examples
#' \dontrun{
#'   download_captcha_rfb()
#'   download_captcha_trt(10, 'my/folder')
#'   download_captcha("https://goo.gl/sqTQ4Z")
#' }
#' @export
download_captcha <- function(url, n = 1, dest = ".", secure = FALSE,
                             .default = "jpeg") {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  safe_download_one <- purrr::safely(download_captcha_one)
  p <- progress::progress_bar$new(total = n)
  result <- purrr::map_chr(seq_len(n), ~{
    result <- safe_download_one(url, dest, secure, .default)
    p$tick()
    as.character(result[!sapply(result, is.null)][[1]])
  })
  invisible(result)
}

download_captcha_one <- function(url, dest, secure, .default) {
  httr::handle_reset(url)
  dest_captcha <- tempfile(pattern = "captcha", tmpdir = dest)
  # Send get request
  r <- httr::GET(
    url, httr::user_agent("R-decryptr"),
    httr::config(ssl_verifypeer = secure),
    httr::timeout(3)
  )
  ct <- r$headers[["content-type"]]
  if (is.null(ct)) ct <- .default
  dest_captcha <- paste0(dest_captcha, ".", basename(ct))
  writeBin(r$content, dest_captcha)
  dest_captcha
}

#' @rdname download_captcha
#' @export
download_captcha_tjrs <- function(n = 1, dest = ".") {
  url <- paste0(
    "http://www.tjrs.jus.br/site_php/consulta",
    "/human_check/humancheck_showcode.php"
  )
  download_captcha(url, dest, n = n)
}

#' @rdname download_captcha
#' @export
download_captcha_tjmg <- function(n = 1, dest = ".") {
  url <- "http://www4.tjmg.jus.br/juridico/sf/captcha.svl"
  download_captcha(url, dest, n = n)
}

#' @rdname download_captcha
#' @export
download_captcha_trt <- function(n = 1, dest = ".") {
  url <- "https://pje.trt4.jus.br/consultaprocessual/seam/resource/captcha"
  download_captcha(url, dest, n = n)
}

#' @rdname download_captcha
#' @export
download_captcha_rfb <- function(n = 1, dest = ".") {
  url <- "http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva/captcha/gerarCaptcha.asp"
  download_captcha(url, dest, n = n)
}

#' @rdname download_captcha
#' @export
download_captcha_tjrj <- function(n = 1, dest = ".") {
  url <- "http://www4.tjrj.jus.br/consultaProcessoWebV2/captcha"
  download_captcha(url, dest, n = n)
}
