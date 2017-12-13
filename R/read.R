
#' @title Read captcha file
#'
#' @description Given the path to a file, reads that file and converts
#' it into a `captcha` object that can be used for classification or
#' decryption. If `ans_in_path = TRUE`, will take the answer for the
#' captcha from the filename and get it ready for modeling.
#'
#' @param path Path to a captcha image
#' @param ans_in_path Whether or not the answer to the captcha is already
#' in the path to the file (separated by and underscore in the filename)
#'
#' @export
read_captcha <- function(path, ans_in_path = FALSE ) {

  # Get extention
  ext <- tolower(tools::file_ext(basename(path)))

  # Load and build captcha object
  if (all(ext %in% c("jpeg", "jpg", "png"))) {

    # Load captcha
    captcha <- grey(load_image(path))

    # Get answer from filename if necessary
    answer <- if (ans_in_path) { get_answer(path) } else { NULL }

    # Create captcha object
    captcha <- list(y = answer, x = captcha)
    class(captcha) <- c("captcha", "prepared")
    attr(captcha, "file") <- path

    return(captcha)
  }

  # Error
  stop("All files must have extensions 'jpeg', 'jpg' or 'png'")
}

#' Read PNG or JPG
#'
#' @param path Path to image
#' @param ... Other arguments passed on to [png::readPNG()]
#' or [jpeg::readJPEG()]
#'
load_image <- function(path, ...) {

  ext <- tolower(tools::file_ext(basename(path)))
  if (ext %in% c("jpeg", "jpg")) {
    img <- jpeg::readJPEG(path, ...)
  } else if (ext == "png") {
    img <- png::readPNG(path, ...)
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
#' @param path Path to a captcha with its answer separated by and
#' underscore ('_') in the filename
#'
get_answer <- function(path) {

  # Collect answer from filename
  answer <- basename(path) %>%
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
