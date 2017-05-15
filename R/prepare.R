#' Predict captcha
#'
#' @param arqs object
#' @param only_x boolean. Is the answers present on file names?
#'
#' @export
prepare <- function(arqs, only_x = FALSE) {
  UseMethod('prepare')
}

#' Prepare captchas
#'
#' Prepare answare and features for modeling. Expect '_' as the answer separator.
#'
#' @param arqs arqs read
#' @param only_x boolean. Is the answers present on file names?
#'
#'@export
prepare.captcha <- function(arqs, only_x = FALSE) {
  x <- plyr::laply(arqs, load_image)
  if (length(arqs) == 1) dim(x) <- c(1, dim(x))
  if (!only_x && length(arqs) > 1) {
    words <- arqs %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      stringr::str_match('_([a-zA-Z0-9]+)$') %>%
      magrittr::extract(TRUE, 2)
    all_letters <- unique(sort(unlist(strsplit(words, ''))))
    y <- plyr::laply(words, create_response, all_letters)
    l <- list(y = y, x = x)
  } else {
    l <- list(y = NULL, x = x)
  }
  class(l) <- 'captcha'
  return(l)
}

create_response <- function(y, all_letters) {
  a <- strsplit(y, '')[[1]]
  n <- length(a)
  n_letters <- length(all_letters)
  if (length(unique(a)) == 1) {
    mm <- matrix(rep(1, n), ncol = 1)
  } else {
    mm <- model.matrix(rep(1, n) ~ a - 1)
  }
  m <- matrix(0L, nrow = n, ncol = n_letters)
  colnames(m) <- all_letters
  sua <- sort(unique(a))
  colnames(mm) <- sua
  m[, sua] <- mm
  # attributes(m) <- list(dim = c(n, n_letters))
  m
}
