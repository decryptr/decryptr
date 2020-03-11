
#' @title Load a captcha-breaking model
#'
#' @description This function can either access the models saved in
#' `decryptrModels:::models` or in a local file create by the user with
#' [train_model()].
#'
#' @param model Either the path to a ".hdf5" file or the name of a known model
#' (`"rfb"`, `"trt"`, `"tjmg"`, `"esaj"`, `"rsc"`, `"cadesp"`, `"nfesp"`, `"tjes"`, `"tjrs"`)
#' @param labs A character vector with all the labels the model could possibly
#' output (necessary when loading a homemade model)
#'
#' @export
load_model <- function(model, labs = c(0:9, base::letters)) {

  # Load model either from a path or from decryptrModels
  if (!(model %in% c("rfb", "trt", "tjmg", "esaj", "rsc", "cadesp", "nfesp", "tjes", "tjrs"))) {
    model <- keras::load_model_hdf5(model, compile = FALSE)
  } else {
    models <- decryptrModels::models
    path <- system.file("keras", package = "decryptrModels")
    files <- dir(path, full.names = TRUE)
    file_path <- normalizePath(files[grepl(model, files)])
    labs <- models[grepl(model, models[["name"]]), ][["labs"]][[1]]
    model <- keras::load_model_hdf5(file_path, compile = FALSE)
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


get_answer_vocab <- function (file, vocab) {
  answer <- basename(file) %>%
    tools::file_path_sans_ext() %>%
    stringr::str_extract("([a-zA-Z0-9]+)$") %>%
    stringr::str_sub(start = 1) %>%
    stringr::str_to_lower() %>%
    stringr::str_split("") %>%
    purrr::flatten_chr()
  if (!is.null(vocab)) answer <- resize_answer(answer, vocab)
  answer
}
data_generator <- function(df, batch_size, shuffle = TRUE) {
  ds <- tensor_slices_dataset(df) %>%
    dataset_map(function(obs) {
      img_str <- tf$read_file(obs$fname)
      img <- tf$image$decode_png(img_str, channels = 3L)
      img <- tf$to_float(img) / 255
      response <- obs$mat
      list(img, response)
    }) %>%
    dataset_repeat()
  ds <- tfdatasets::dataset_padded_batch(
    dataset = ds,
    batch_size = batch_size,
    padded_shapes = list(shape(50L, 180L, 3L), shape(6L, 36L))
  )
  if (shuffle) ds <- dataset_shuffle(ds, buffer_size = 100)
  ds
}


#' @title Craete a model to break image captchas using keras using data generators
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
#' @param mult multiplier to convolutional channels
#' @param batch_size Number of observations to consider in each iteration
#' @param path Path where to save model (if `NULL` won't save it)
#' @param verbose Print model specification
#'
#' @import keras
#' @import tfdatasets
train_model_generator <- function(data,
                                  frac_test = 0.1,
                                  n_epochs = 30,
                                  n_units = 256,
                                  mult = 2,
                                  batch_size = 64,
                                  path = "./model.hdf5",
                                  verbose = TRUE) {

  # Check whether frac_test is less than or equal to 100%
  stopifnot(frac_test <= 1.0)
  # Get absolute numbers
  n_tot <- length(data)
  n_test <- n_tot * frac_test

  data_dims <- join_captchas(data[1:2])
  vocab <- c(letters, 0:9)

  df <- list(
    fname = as.character(data),
    mat = data %>%
      purrr::map(get_answer, vocab = vocab) %>%
      plyr:::list_to_array()
  )
  ds_train <- list(
    fname = df$fname[id_train],
    mat = df$mat[id_train,,,drop = FALSE]
  ) %>% data_generator(32, TRUE)

  ds_test <- list(
    fname = df$fname[-id_train],
    mat = df$mat[-id_train,,,drop = FALSE]
  ) %>% data_generator(32, TRUE)

  # Create sample for training
  my_sample <- sample(seq_len(n_tot), n_tot - n_test, replace = FALSE)

  # Create model
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(
      input_shape = dim(data_dims$x)[-1],
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
    layer_dense(units = prod(dim(data_dims$y)[-1])) %>%
    layer_reshape(target_shape = dim(data_dims$y)[-1]) %>%
    layer_activation("softmax")

  # Print model if necessary
  if (verbose) { print(model) }

  # Compile and fit model

  # Compile model
  model %>% compile(
    loss = loss_categorical_crossentropy,
    optimizer = optimizer_sgd(0.5),
    metrics = c("accuracy")
  )

  # Train model
  model %>% fit_generator(
    generator = ds_train,
    steps_per_epoch = 200,
    epochs = n_epochs,
    validation_data = ds_test,
    validation_steps = 20
  )

  # Save model if necessary
  if (!is.null(path)) { keras::save_model_hdf5(model, path) }

  # Create model object
  out <- list(model = model, labs = dimnames(data_dims$y)[[3]])
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
#' @export
join_captchas <- function(captchas) {

  # Separate captchas into xs and ys
  captchas_t <- purrr::transpose(captchas)
  xs <- captchas_t$x; ys <- captchas_t$y

  # Join ys
  ys <- join_answers(ys)

  # Create a matrix to contain all xs
  xs <- abind::abind(xs, along = 0.1)

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

  if (is.character(ys[[1]])) {

    # Get all distinct letters and letters per captcha
    vocab <- ys %>%
      purrr::flatten_chr() %>%
      unique() %>% sort()

    # Get length of answers
    n <- purrr::map_dbl(ys, length)
    if (!all(n == n[1])) {
      stop("Answers to all captchas must have the same length")
    } else { n <- n[1] }

    # Apply resizing to all list of answers
    ys <- purrr::map(ys, resize_answer, vocab)

  }

  # Join answers
  abind::abind(ys, along = 0.1)
}
