
#' @title Break captchas
#'
#' @description Given one or more captchas and a model, returns the
#' text that is supposed to break the captchas.
#'
#' @param captcha The paths to one or more captchas or a vector of
#' captchas read with [read_captcha()]
#' @param model Either a model, the path to a model or the name of a
#' known model (see [train_model()] and [load_model()])
#'
#' @export
decrypt <- function(captcha, model) {

  # Read captcha and model if necessary
  if (is.character(model)) { model <- load_model(model) }
  if (is.character(captcha)) { captcha <- read_captcha(captcha) }

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
  x <- array(dim = c(1, nrow(captcha$x), ncol(captcha$x), 1))
  x[1,,,] <- captcha$x

  # Predict captcha with model
  pred_ids <- predict(model$model, x) %>%
    apply(c(1, 2), which.max) %>%
    as.vector()

  # Output answer
  stringr::str_c(model$labs[pred_ids], collapse = "")
}
