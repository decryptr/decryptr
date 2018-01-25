
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
#'
#' @return A list of captcha objects
#'
#' @export
read_captcha <- function(file, ans_in_path = FALSE) {

  # Check if files are images
  ext <- tolower(tools::file_ext(basename(file)))
  stopifnot(all(ext %in% c("jpeg", "jpg", "png")))

  # Iterate over files
  out <- purrr::map(file, read_captcha_, ans_in_path)
  class(out) <- c("captcha")

  return(out)
}

#' Read a captcha file
#'
#' @param file Path to a captcha image
#' @param ans_in_path Whether or not the answer to the captcha is already
#' in the path to the file (separated by and underscore in the filename)
#'
read_captcha_ <- function(file, ans_in_path) {

  # Load captcha
  captcha <- grey(load_image(file))

  # Get answer from filename if necessary
  answer <- if (ans_in_path) { get_answer(file) } else { NULL }

  # Create captcha object
  captcha <- list(y = answer, x = captcha)
  attr(captcha, "file") <- file

  return(captcha)
}

#' Read PNG or JPG
#'
#' @param file Path to image
#' @param ... Other arguments passed on to [png::readPNG()]
#' or [jpeg::readJPEG()]
#'
load_image <- function(file, ...) {

  ext <- tolower(tools::file_ext(basename(file)))
  if (ext %in% c("jpeg", "jpg")) {
    img <- jpeg::readJPEG(file, ...)
  } else if (ext == "png") {
    img <- png::readPNG(file, ...)
  }

  return(img)
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
#'
get_answer <- function(file) {

  # Collect answer from filename
  answer <- basename(file) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_extract("_([a-zA-Z0-9]+)$") %>%
    stringr::str_sub(start = 2) %>%
    stringr::str_to_lower() %>%
    stringr::str_split("") %>%
    purrr::flatten_chr()

  # Build answer matrix
  mm <- model.matrix(rep(1, length(answer)) ~ answer - 1)
  attr(mm, "assign") <- NULL; attr(mm, "contrasts") <- NULL
  colnames(mm) <- unique(sort(answer))

  return(mm)
}
