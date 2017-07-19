#' Classify captcha
#'
#' @param captcha object
#' @param ... other
#'
#' @export
classify <- function(captcha, ...) {
  UseMethod("classify")
}

#' Classify captchas
#'
#' @param captcha object
#' @param dest destination
#' @param answer answer
#' @param ... other
#'
#' @export
classify.captcha <- function(captcha, dest = dirname(captcha)[1],
                             answer = NULL, ...) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(answer)) {
    if (length(answer) != length(captcha))
      stop("When answer is not null, captcha
           files and answers must have same length.")
    out <- purrr::map2_chr(captcha, answer,
                           ~classify_one(read_captcha(.x), dest, .y))
  } else {
    out <- purrr::map_chr(captcha, ~classify_one(
      read_captcha(.x), dest, answer = NULL, ...)
    )
  }
  invisible(out)
}

classify_one <- function(captcha, dest, answer = NULL, ...) {
  model <- list(...)$model
  if (is.null(answer)) {
    graphics::plot(captcha)
    if (!is.null(model)) {
      pred <- predict(model, arq = captcha)
      pr <- sprintf("Answer (%s): ", pred)
    } else {
      pr <- "Answer: "
    }
    answer <- readline(prompt = pr)
    if (answer == '' && !is.null(model)) answer <- pred
    # if (runif(1) < .1) cat(praise::praise(), "\n")
  }
  # print(answer)
  nm <- tools::file_path_sans_ext(basename(captcha))
  ext <- tools::file_ext(basename(captcha))
  out <- sprintf("%s/%s_%s.%s", dest, nm, answer, ext)
  file.copy(captcha, out)
  class(out) <- c("captcha")
  invisible(out)
}
