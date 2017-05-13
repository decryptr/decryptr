
#' Predizer os numeros do arquivo
#'
#' @param object object
#' @param ... other
#'
#' @export
predict.tjrs <- function(object, ...) {
  m <- captchaTJRS:::m
  object %>%
    arrumar(names(m$trainingData)) %>% {
      caret::predict.train(m, newdata = ., type = 'prob') %>%
        tibble::rownames_to_column() %>%
        tidyr::gather(key, value, -rowname) %>%
        dplyr::group_by(rowname) %>%
        dplyr::summarise(v = key[which.max(value)]) %>%
        with(v)
    } %>%
    paste(collapse = '')
}
