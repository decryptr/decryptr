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
  caret::predict.train(m, newdata = newdata) %>%
    paste(collapse = '')
}
