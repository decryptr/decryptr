# predict.tjrs <- function(object, preprocess = preprocess_tjrs, ...) {
#   m <- captchaTJRS:::m
#   newdata <- preprocess_tjrs(object, nm = names(m$trainingData))
#   predict(m, newdata = newdata) %>%
#     paste(collapse = '')
# }

#' Predizer os numeros do arquivo
#'
#' @param object keras model
#' @param ... captcha file
#'
#' @export
predict.captcha <- function(object, ...) {
  arq <- list(...)$arq
  predict(object, newdata = prepare(arq))
}

# break_captcha.tjrs <- function(arq, model = NULL) {
#   a <- read_tjrs(arq)
#   if (is.null(modelo)) {
#     model <- decryptr.models::tjrs
#   }
#   X <- prepare(arq)
#   predict(model, newdata = X)
# }
