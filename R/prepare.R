#' Predict captcha
#'
#' @param arqs object
#' @param only_x boolean. Is the answers present on file names?
#'
#' @export
prepare <- function(arqs, only_x = FALSE) {
  UseMethod('prepare')
}

#' Predict captcha
#'
#' @param arqs object
#' @param only_x boolean. Is the answers present on file names?
#'
#' @export
prepare.raw <- function(arqs, only_x = TRUE) {
  # works only for single file
  im0 <- NULL
  try(im0 <- png::readPNG(arqs), silent = TRUE)
  if (is.null(im0)) im0 <- jpeg::readJPEG(arqs)
  dim0 <- dim(im0)
  X <- array(NA_real_, dim = c(1, dim0[1], dim0[2], 1))
  X[1,,,] <- cinza(im0)
  l <- list(y = NULL, x = X)
  class(l) <- 'prepared'
  return(l)
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
  x <- prepare_x(arqs)
  if (!only_x && length(arqs) > 1) {
    words <- arqs %>%
      basename() %>%
      tools::file_path_sans_ext() %>%
      stringr::str_match('_([a-zA-Z0-9]+)$') %>%
      magrittr::extract(TRUE, 2) %>%
      tolower()
    all_letters <- unique(sort(unlist(strsplit(words, ''))))
    y <- plyr::laply(words, create_response, all_letters)
    l <- list(y = y, x = x)
  } else {
    l <- list(y = NULL, x = x)
  }
  l$n <- nrow(x)
  class(l) <- 'prepared'
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

# prepare_x <- function(arqs) {
#   x <- abind::abind(purrr::map(arqs, load_image), along = .1)
#   (x[,,,1, drop = FALSE] + x[,,,2, drop = FALSE] + x[,,,3, drop = FALSE]) / 3
# }

cinza <- function(im) {
  (im[,,1, drop = FALSE] + im[,,2, drop = FALSE] + im[,,3, drop = FALSE]) / 3
}

prepare_x <- function(arqs) {
  im0 <- load_image(arqs[1])
  dim0 <- dim(im0)
  X <- array(NA_real_, dim = c(length(arqs), dim0[1], dim0[2], 1))
  X[1,,,] <- cinza(im0)
  for (i in seq_along(arqs)[-1]) {
    im <- load_image(arqs[i])
    X[i,,,] <- cinza(im)
  }
  X
}

print.prepared <- function(x) {
  str(x)
}

