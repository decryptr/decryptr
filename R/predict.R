#' @title Break a captcha
#'
#' @description Given a captcha and a model, returns the text that
#' is supposed to break the captcha
#'
#' @param captcha The path to a captcha or a captcha read with
#' [read_captcha()]
#' @param model The name of a model or a model from `decryptrModels`
#' (see [read_model()])
#'
#' @rdname decrypt
#' @export
decrypt <- function(captcha, model) {
  UseMethod("decrypt")
}

#' @rdname decrypt
#' @export
decrypt.character <- function(captcha, model) {
  if (is.character(model)) { model <- read_model(model) }
  decrypt.captcha(read_captcha(captcha), model)
}

#' @rdname decrypt
#' @export
decrypt.captcha <- function(captcha, model) {
  if (is.character(model)) { model <- read_model(model) }

  X <- captcha$x
  pred_ids <- predict(model$model, X) %>%
    apply(c(1, 2), which.max) %>%
    as.vector()
  paste(model$labs[pred_ids], collapse = "")
}
