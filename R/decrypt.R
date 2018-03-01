
#' @title Break captchas
#'
#' @description Given one or more captchas and a model, returns the
#' text that is supposed to break the captchas.
#'
#' @param captcha The paths to one or more captchas or a list of
#' captchas read with [read_captcha()] or a raw scalar - used for web API's.
#' @param model Either a model, the path to a model or the name of a
#' known model (see [train_model()] and [load_model()])
#'
#' @export
decrypt <- function(captcha, model) {

  # Read captcha and model if necessary
  if (is.character(model)) { model <- load_model(model) }
  if (is.character(captcha)) {
    captcha <- read_captcha(captcha)
  } else if (is.raw(captcha)) {
    captcha <- read_captcha_raw(captcha)
  }

  # Iterate over each captcha
  purrr::map_chr(captcha, decrypt_, model)
}

#' Break a captcha
#'
#' @param captcha A captcha read with [read_captcha()]
#' @param model A model loaded with [load_model()]
#'
decrypt_ <- function(captcha, model) {

  # Reshape captcha so that it works with model
  x <- array(dim = c(1, dim(captcha$x)))
  x[1,,,] <- captcha$x

  # Predict captcha with model
  pred_ids <- predict(model$model, x) %>%
    apply(c(1, 2), which.max) %>%
    as.vector()

  # Output answer
  stringr::str_c(model$labs[pred_ids], collapse = "")
}

#' @title Break captchas
#'
#' @description This is an alternative to [decrypt()] that can be
#' used in a more convenient way if you're used to modeling. So,
#' if you're a hardcore #rstats fan and your fingers start itching
#' if you don't run [stats::predict()] every 2 lines, this wrapper
#' is for you.
#'
#' @param object A model for which prediction is desired
#' @param newdata A list of `captcha` objects read with [read_captcha()]
#' @param ... -
#'
#' @export
predict.captcha <- function(object, newdata = NULL, ...) {
  decrypt(newdata, object)
}
