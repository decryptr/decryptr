decryptr_path_to_txt <- function(img) {
  img %>%
    fs::path_ext_remove() %>%
    fs::path_file() %>%
    stringr::str_extract("(?<=_)([a-zA-Z0-9]+)$") %>%
    stringr::str_to_lower() %>%
    stringr::str_split("")
}

decryptr_txt_to_int <- function(x, vocab) {
  as.integer(factor(x, levels = vocab)) - 1L
}

decryptr_get_answer_tf <- function(y, n_vocab) {
  tensorflow::tf$one_hot(y, n_vocab)
}

decryptr_img_load_tf <- function(x, rsz) {
  x %>%
    tensorflow::tf$io$read_file() %>%
    tensorflow::tf$image$decode_png() %>%
    tensorflow::tf$image$convert_image_dtype(dtype = tensorflow::tf$float32) %>%
    tensorflow::tf$image$resize(rsz) %>%
    tensorflow::tf$image$rgb_to_grayscale()
}

decryptr_prep_img <- function(dt, rsz, n_vocab) {
  dt$img <- decryptr_img_load_tf(dt$img, rsz)
  dt$y <- decryptr_get_answer_tf(dt$y, n_vocab)
  unname(dt)
}

captcha_preprocess <- function(dt_input, n_vocab, rsz, batch_size, gpu = TRUE) {

  if (missing(n_vocab)) n_vocab <- max(unlist(dt_input$y))
  prep <- purrr::partial(decryptr_prep_img, rsz = rsz, n_vocab = n_vocab)

  dt_input %>%
    tfdatasets::tensor_slices_dataset() %>%
    tfdatasets::dataset_map(prep) %>%
    tfdatasets::dataset_batch(batch_size) %>%
    tfdatasets::dataset_prefetch_to_device("/gpu:0")

  # if (gpu) {
  #   dt_input %>%
  #
  # } else {
  #   dt_input %>%
  #     tfdatasets::dataset_prefetch(1L)
  # }
  #
  # dt_input

}

#' Prepare dataset to run keras models
#'
#' @param dir directory
#' @param n_train number of training examples
#' @param vocab captcha vocabulary. If missing will be inferred from dataset
#' @param batch_size size of the batch
#' @param prep_options options to prepare captcha, returned
#'   by [captcha_prep_options()]
#'
#' @export
captcha_prepare_dataset <- function(dir, n_train, vocab, batch_size, prep_options) {

  # X and y
  img <- as.character(fs::dir_ls(dir))
  y <- img %>%
    decryptr_path_to_txt() %>%
    purrr::map(decryptr_txt_to_int, vocab)

  prop_train <- n_train / length(img)

  # data split
  data <- tibble::tibble(img = img, y = y) %>%
    rsample::initial_split(prop_train)

  n_vocab <- length(vocab)

  # tf datasets prep
  train_dataset <- data %>%
    rsample::training() %>%
    captcha_preprocess(
      n_vocab = n_vocab,
      rsz = prep_options$img_size,
      batch_size
    )

  test_dataset <- data %>%
    rsample::testing() %>%
    captcha_preprocess(
      n_vocab = n_vocab,
      rsz = prep_options$img_size,
      batch_size
    )

  shapes <- train_dataset %>%
    tfdatasets::output_shapes() %>%
    purrr::map(~.x$as_list()[-1]) %>%
    purrr::set_names(c("x", "y"))

  list(
    training = train_dataset,
    validation = test_dataset,
    shape = shapes
  )

}

#' Options to prepare captcha
#'
#' @param img_size image size
#'
#' @export
captcha_prep_options <- function(img_size = c(24L, 72L)) {
  list(
    img_size = img_size
  )
}
