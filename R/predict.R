
#' @title Break a captcha
#'
#' @description Given a captcha and a model, returns the text that
#' is supposed to break the captcha.
#'
#' @param captcha The path to a captcha or a captcha read with
#' [read_captcha()]
#' @param model The name of a model or a model from `decryptrModels`
#' (see [load_model()] or [train_model()])
#'
#' @export
decrypt <- function(captcha, model) {

  # Read captcha and model if necessary
  if (is.character(model)) { model <- load_model(model) }
  if (is.character(captcha)) { captcha <- read_captcha(captcha) }

  # Reshape captcha so that it works with model
  x <- array(dim = c(1, nrow(captcha$x), ncol(captcha$x), 1))
  x[1,,,] <- captcha$x

  # Predict captcha with model
  pred_ids <- predict(model$model, x) %>%
    apply(c(1, 2), which.max) %>%
    as.vector()

  # Output answer
  stringr::str_c(model$labs[pred_ids], collapse = "")
}
