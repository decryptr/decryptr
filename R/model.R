model <- function(prepared, ...) {
  UseMethod("model")
}

#' Model image captcha using keras
#'
#' Tries to adjust an automatic model using keras
#'
#' @param prepared_data prepared data
#' @param n_test test data size
#' @param n_epochs maximum number of epochs
#' @param n_units number of units to consider in dense layer
#' @param batch_size number of observations to consider in each iteration
#' @param save_model logical. save model on disk?
#' @param save_path path to save model
#' @param verbose print model specification
#'
#' @import keras
#' @export
model.captcha <- function(prepared_data,
                          n_test = prepared_data$n * 0.1,
                          n_epochs = 30,
                          n_units = 256,
                          batch_size = 64,
                          save_model = TRUE,
                          save_path = "model.hdf5",
                          verbose = TRUE) {
  n_tot <- prepared_data$n
  if (n_tot < n_test) stop("n_test should be less than your data rows.")
  my_sample <- sample(seq_len(n_tot), n_tot - n_test, replace = FALSE)
  ################################################
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(
      input_shape = dim(prepared_data$x)[-1],
      filters = 16,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu"
    ) %>%
    layer_max_pooling_2d() %>%
    layer_conv_2d(
      filters = 32,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu"
    ) %>%
    layer_max_pooling_2d() %>%
    layer_conv_2d(
      filters = 64,
      kernel_size = c(5, 5),
      padding = "same",
      activation = "relu"
    ) %>%
    layer_max_pooling_2d() %>%
    layer_flatten() %>%
    layer_dense(units = n_units) %>%
    layer_dropout(.1) %>%
    layer_dense(units = prod(dim(prepared_data$y)[-1])) %>%
    layer_reshape(target_shape = dim(prepared_data$y)[-1]) %>%
    layer_activation("softmax")
  if (verbose) print(model)
  ################################################
  model %>%
    compile(
      optimizer = "adam",
      loss = "categorical_crossentropy",
      metrics = "accuracy"
    )
  model %>%
    fit(
      x = prepared_data$x[my_sample, , , , drop = FALSE],
      y = prepared_data$y[my_sample, , , drop = FALSE],
      batch_size = batch_size,
      epochs = n_epochs,
      shuffle = TRUE,
      validation_data = list(
        prepared_data$x[-my_sample, , , , drop = FALSE],
        prepared_data$y[-my_sample, , , drop = FALSE]
      )
    )
  if (save_model) save_model_hdf5(model, save_path)
  ################################################
  object <- list(model = model, labs = dimnames(prepared_data$y)[[3]])
  class(object) <- "captcha"
  object
}
