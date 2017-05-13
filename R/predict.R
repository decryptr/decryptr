#' Predict captcha
#'
#' @param object object
#' @param ... other
#'
#' @export
predict <- function(object, ...) {
  UseMethod('predict')
}

#' Predizer os numeros do arquivo
#'
#' @param object object
#' @param preprocess preprocess function
#' @param ... other
#'
#' @export
predict.tjrs <- function(object, preprocess = preprocess_tjrs, ...) {
  m <- captchaTJRS:::m
  newdata <- preprocess_tjrs(object, nm = names(m$trainingData))
  predict(m, newdata = newdata) %>%
    paste(collapse = '')
}

predict.captcha <- function(model, object, ...) {
  predict(model, newdata = prepare(object))
}

break_captcha.tjrs <- function(arq, model = NULL) {
  a <- read_tjrs(arq)
  if (is.null(modelo)) {
    model <- decryptr.models::tjrs
  }
  X <- prepare(arq)
  predict(model, newdata = X)
}
