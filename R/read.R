
#' @title Read captcha files
#'
#' @description Given the paths to one or more files, reads and converts
#' them into a `captcha` list that can be used for classification or
#' decryption. If `ans_in_path = TRUE`, will take the answer for the
#' captchas from their filenames and get them ready for modeling.
#'
#' @param file Paths to one or more captcha images
#' @param ans_in_path Whether or not the answers to the captchas are already
#' in the paths to the files (separated by and underscore in the filename)
#' @param vocab Character vector with all possible values in the captcha. If
#' not specified, answers won't be transformed to the matrix format.
#'
#' @return A list of captcha objects
#'
#' @export
read_captcha <- function(file, ans_in_path = FALSE, vocab = NULL) {

  # Check if files are images
  ext <- tolower(tools::file_ext(basename(file)))
  stopifnot(all(ext %in% c("jpeg", "jpg", "png")))

  # Iterate over files
  out <- purrr::map(file, read_captcha_, ans_in_path, vocab)
  class(out) <- c("captcha")

  return(out)
}

#' Read a captcha file
#'
#' @param file Path to a captcha image
#' @param ans_in_path Whether or not the answer to the captcha is already
#' in the path to the file (separated by and underscore in the filename)
#' @param vocab Character vector with all possible values in the captcha. If
#' not specified, answers won't be transformed to the matrix format.
#'
read_captcha_ <- function(file, ans_in_path, vocab) {

  # Load captcha
  captcha <- load_image(file)
  if (length(dim(captcha)) == 2) captcha <- array(captcha, dim = c(dim(captcha), 3))
  captcha <- grey(captcha)

  # Get answer from filename if necessary
  answer <- if (ans_in_path) { get_answer(file, vocab) } else { NULL }

  # Create captcha object
  captcha <- list(y = answer, x = captcha)
  attr(captcha, "file") <- file
  class(captcha) <- c("captcha")

  return(captcha)
}

#' Read PNG or JPG
#'
#' @param file Path to image
#' @param ... Other arguments passed on to [magick::image_read()]
#'
load_image <- function(file, ...) {
  magick::image_read(file, ...) %>%
    magick::image_data('rgba') %>%
    as.numeric()
}

#' Convert image to greyscale
#'
#' @param img An image read with [load_image()]
#'
grey <- function(img) {
  (img[,,1,drop = FALSE]+img[,,2,drop = FALSE]+img[,,3,drop = FALSE])/3
}

#' Get answer from filename and build answer matrix
#'
#' @param file Path to a captcha with its answer separated by and
#' underscore ('_') in the filename
#' @param vocab Character vector with all possible values in the captcha. If
#' not specified, answers won't be transformed to the matrix format.
#'
get_answer <- function(file, vocab) {

  if (any(stringr::str_detect(vocab, "A-Z")))
    upper_case <- TRUE
  else
    upper_case <- FALSE


  # Collect answer from filename
  answer <- basename(file) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_extract("_([a-zA-Z0-9]+)$") %>%
    stringr::str_sub(start = 2)

  if (!upper_case)
    answer <- stringr::str_to_lower(answer)

  answer <- answer %>%
    stringr::str_split("") %>%
    purrr::flatten_chr()

  # add vocabulary
  if (!is.null(vocab)) answer <- resize_answer(answer, vocab)

  return(answer)
}

resize_answer <- function(answer, vocab) {
  answer <- factor(answer, levels = vocab)
  # Build answer matrix
  mm <- model.matrix(rep(1, length(answer)) ~ answer - 1)
  attr(mm, "assign") <- NULL; attr(mm, "contrasts") <- NULL
  colnames(mm) <- levels(answer)

  return(mm)
}

read_captcha_raw <- function(x) {

  img <- try_read_jpeg(x)
  if (!is.array(img)) {
    img <- try_read_png(x)
  }
  if (!is.array(img)) {
    stop("decryptr doesn't know how to handle this kind of image")
  }

  captcha <- list(list(x = grey(img), y = NULL))
  class(captcha) <- "captcha"
  captcha
}

try_read_jpeg <- purrr::possibly(load_image, otherwise = NA, quiet = TRUE)

try_read_png <- purrr::possibly(load_image, otherwise = NA, quiet = TRUE)



