model <- function(prepared, ...) {
  UseMethod('model')
}

#' Model image captcha using keras
#'
#' Model image captcha using keras
#'
#' @param prepared prepared data
#' @param epochs maximum number of epochs
#'
#' #@import keras
#' @export
model.captcha <- function(prepared, epochs = 100) {
  ################################################
  # Keras package is not public yet.
  ################################################
  n_tot <- nrow(prepared$y)
  n_test <- 200
  my_sample <- sample(seq_len(n_tot), n_tot - n_test, replace = FALSE)
  n_valid <- 20

  x_train <- prepared$x[my_sample,,,]
  y_train <- prepared$y[my_sample,,]

  x_test <- prepared$x[-my_sample,,,]
  y_test <- prepared$y[-my_sample,,]

  x_valid <- x_test[seq_len(n_valid),,,]
  y_valid <- y_test[seq_len(n_valid),,]

  x_test <- x_test[-seq_len(n_valid),,,]
  y_test <- y_test[-seq_len(n_valid),,]
  ################################################

  # model <- keras_model_sequential()
  # model %>%
  #   layer_conv_2d(
  #     input_shape = dim(x_train)[-1],
  #     filters = 3 * 3, kernel_size = c(3,3),
  #     padding = "same",
  #     activation = "relu",
  #     strides = list(1, 1)
  #   ) %>%
  #   layer_conv_2d(
  #     filters = 3 * 3, kernel_size = c(3,3),
  #     padding = "same",
  #     activation = "relu"
  #   ) %>%
  #   layer_max_pooling_2d() %>%
  #   layer_conv_2d(
  #     filters = 2 * 2, kernel_size = c(2,2),
  #     padding = "same",
  #     activation = "relu"
  #   ) %>%
  #   layer_max_pooling_2d() %>%
  #   layer_reshape(c(
  #     dim(y_train)[2],
  #     prod(unlist(.$output_shape)) / dim(y_train)[2])
  #   ) %>%
  #   bidirectional(
  #     layer_lstm(units = 256, return_sequences = TRUE),
  #     merge_mode = "concat"
  #   ) %>%
  #   layer_dropout(.2) %>%
  #   layer_dense(64, activation = "relu") %>%
  #   layer_dropout(.1) %>%
  #   layer_dense(32, activation = "relu") %>%
  #   layer_dropout(.2) %>%
  #   layer_dense(dim(y_train)[3], activation = "relu") %>%
  #   layer_activation("softmax")
  #
  # model %>%
  #   compile(
  #     optimizer = "adagrad",
  #     loss = "categorical_crossentropy",
  #     metrics = "accuracy"
  #   )
  #
  # model %>%
  #   fit(
  #     x = x_train, y = y_train,
  #     batch_size = round(sqrt(nrow(y_train))), epochs = epochs,
  #     validation_data = list(x_test, y_test)
  #   )

  object <- list(model = model, labs = dimnames(y_train)[[3]])
  class(object) <- 'captcha'
  object
}
