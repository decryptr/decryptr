model <- function(prepared, ...) {
  UseMethod('model')
}

#' Model image captcha using keras
#'
#' Tries to adjust an automatic model using keras
#'
#' @param prepared prepared data
#' @param epochs maximum number of epochs
#' @param prop_test test proportion
#' @param drop_out size of the dropout
#' @param n_filters number of filters on conv net
#' @param window_size conv net window size
#' @param pooling_size max pooling window size
#' @param verbose print model specification
#'
#' @import keras
#' @export
model.captcha <- function(prepared_data,
                          epochs = 100,
                          prop_test = .3,
                          drop_out = .8,
                          n_filters = 3,
                          window_size = 5,
                          pooling_size = 3,
                          verbose = TRUE) {
  n_tot <- nrow(prepared_data$y)
  n_test <- round(nrow(prepared_data$y) * prop_test)
  my_sample <- sample(seq_len(n_tot), n_tot - n_test, replace = FALSE)
  x_train <- prepared_data$x[my_sample,,,]
  y_train <- prepared_data$y[my_sample,,]
  x_test <- prepared_data$x[-my_sample,,,]
  y_test <- prepared_data$y[-my_sample,,]
  ################################################
  model <- keras_model_sequential()
  model %>%
    layer_conv_2d(
      input_shape = dim(x_train)[-1],
      filters = dim(y_train)[2] * n_filters,
      kernel_size = rep(window_size, 2),
      activation = "relu"
    ) %>%
    layer_max_pooling_2d(rep(pooling_size, 2)) %>%
    layer_conv_2d(
      input_shape = dim(x_train)[-1],
      filters = dim(y_train)[2] * n_filters,
      kernel_size = rep(window_size, 2),
      activation = "relu"
    ) %>%
    layer_max_pooling_2d(rep(pooling_size, 2)) %>%
    layer_reshape(list(
      dim(y_train)[2],
      floor(prod(unlist(.$output_shape)) / dim(y_train)[2])
    )) %>%
    bidirectional(layer_lstm(units = 1024, return_sequences = TRUE)) %>%
    layer_dropout(drop_out) %>%
    layer_dense(dim(y_train)[3], activation = "relu") %>%
    layer_activation("softmax") %>%
    compile(
      optimizer = "adagrad",
      loss = "categorical_crossentropy",
      metrics = "accuracy"
    )
  if (verbose) print(model)
  ################################################
  model %>%
    fit(
      x = x_train,
      y = y_train,
      batch_size = 100,
      epochs = epochs,
      validation_data = list(x_test, y_test)
    )
  ################################################
  object <- list(model = model, labs = dimnames(y_train)[[3]])
  class(object) <- 'captcha'
  object
}
