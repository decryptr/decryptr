
#' @title Load a captcha-breaking model
#'
#' @description This function can either access the models saved in
#' `decryptrModels:::models` or in a local file create by the user with
#' [train_model()].
#'
#' @param model Either the path to a ".hdf5" file or the name of a known model
#' (`"rfb"`, `"trt"`, `"tjmg"` or `"esaj"`)
#' @param labs A character vector with all the labels the model could possibly
#' output (necessary when loading a homemade model)
#'
#' @export
load_model <- function(model, labs = c(0:9, base::letters)) {

  # Load model either from a path or from decryptrModels
  if (!(model %in% c("rfb", "trt", "tjmg", "esaj"))) {
    model <- keras::load_model_hdf5(model)
  } else {
    models <- decryptrModels:::models
    path <- system.file("keras", package = "decryptrModels")
    files <- dir(path, full.names = TRUE)
    file_path <- normalizePath(files[grepl(model, files)])
    labs <- models[grepl(model, models[["name"]]), ][["labs"]][[1]]
    model <- keras::load_model_hdf5(file_path)
  }

  # Create model object
  m <- list(labs = labs, model = model)
  class(m) <- "model"

  return(m)
}

#' @title Craete a model to break image captchas using keras
#'
#' @description This function tries to adjust an automatic model for
#' breaking captchas using the `rstudio/keras` package. It creates a
#' [keras::keras_model_sequential()] and learns from the inputed
#' labeled data.
#'
#' @param data A list of previously-classified captchas read with
#' `read_captcha(..., ans_in_path = TRUE)` (see [classify()])
#' @param frac_test Fraction of data used for testing
#' @param n_epochs Maximum number of epochs
#' @param n_units Number of units to consider in dense layer
#' @param batch_size Number of observations to consider in each iteration
#' @param path Path where to save model (if `NULL` won't save it)
#' @param verbose Print model specification
#'
#' @import keras
#' @export
train_model <- function(data, frac_test = 0.1, n_epochs = 30, n_units = 256,
                        batch_size = 64, path = "./model.hdf5", verbose = TRUE) {

  # Check whether frac_test is less than or equal to 100%
  stopifnot(frac_test <= 1.0)

  # Convert list of captchas to expected format
  data <- join_captchas(data)

  # Get absolute numbers
  n_tot <- data$n
  n_test <- n_tot*frac_test

  # Create sample for training
  my_sample <- sample(seq_len(n_tot), n_tot - n_test, replace = FALSE)

  # Create model
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(
      input_shape = dim(data$x)[-1],
      filters = 16,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu") %>%
    layer_max_pooling_2d() %>%
    layer_conv_2d(
      filters = 32,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu") %>%
    layer_max_pooling_2d() %>%
    layer_conv_2d(
      filters = 64,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu") %>%
    layer_max_pooling_2d() %>%
    layer_flatten() %>%
    layer_dense(units = n_units) %>%
    layer_dropout(.1) %>%
    layer_dense(units = prod(dim(data$y)[-1])) %>%
    layer_reshape(target_shape = dim(data$y)[-1]) %>%
    layer_activation("softmax")

  # Print model if necessary
  if (verbose) { print(model) }

  # Compile and fit model
  model %>%
    compile(
      optimizer = "adam",
      loss = "categorical_crossentropy",
      metrics = "accuracy")
  model %>%
    fit(
      x = data$x[my_sample, , , , drop = FALSE],
      y = data$y[my_sample, , , drop = FALSE],
      batch_size = batch_size,
      epochs = n_epochs,
      shuffle = TRUE,
      validation_data = list(
        data$x[-my_sample, , , , drop = FALSE],
        data$y[-my_sample, , , drop = FALSE]))

  # Save model if necessary
  if (!is.null(path)) { keras::save_model_hdf5(model, path) }

  # Create model object
  out <- list(model = model, labs = dimnames(data$y)[[3]])
  class(out) <- "model"

  return(out)
}

#' Join captcha objects
#'
#' @description Given a list of captcha objetcs, joins them in the
#' shape expected by the model of [train_model()].
#'
#' @param captchas A list of answered captchas (read by
#' [read_captcha()])
#'
join_captchas <- function(captchas) {

  # Separate captchas into xs and ys
  captchas_t <- purrr::transpose(captchas)
  xs <- captchas_t$x; ys <- captchas_t$y

  # Join ys
  ys <- join_answers(ys)

  # Create a matrix to contain all xs
  xs <- array(
    purrr::reduce(xs, c),
    dim = c(length(xs), dim(xs[[1]])[1], dim(xs[[1]])[2], 1))

  # Return a list with xs and ys
  return(list(y = ys, x = xs, n = dim(xs)[1]))
}

#' Join answer matrixes
#'
#' @description Given a list of answer matrixes, joins them in the
#' shape expected by the model of [train_model()].
#'
#' @param ys A list of answer matrixes (captcha$y)
#'
join_answers <- function(ys) {

  # Resize an answer so that it is as wide as possible
  resize_answer <- function(y, letters, n) {

    # Create wide matrix
    m <- matrix(0L, n, length(letters))
    colnames(m) <- letters

    # Add y to wide matrix
    what <- sort(unique(attr(y, "dimnames")[[2]]))
    m[, what] <- y

    return(m)
  }

  # Get all distinct letters and letters per captcha
  letters <- ys %>%
    purrr::map(~attr(.x, "dimnames")[[2]]) %>%
    purrr::flatten_chr() %>%
    unique() %>% sort()
  n <- nrow(ys[[1]])

  # Apply resizing to all list of answers
  ys <- purrr::map(ys, resize_answer, letters, n)

  # Join answers
  plyr::laply(ys, function(x) { x })
}
