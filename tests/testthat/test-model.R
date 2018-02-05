context("model")

test_that("reading models works", {

  # Setup
  models <- list(
    load_model("rfb"),
    load_model("trt"),
    load_model("tjmg"),
    load_model("esaj"))

  # Calculate object size
  objsz <- function(x) { as.integer(object.size(x)) }

  # Expectations
  expect_equal(class(models[[1]]), "model")
  expect_equal(objsz(models[[1]]), 4544)
  expect_equal(class(models[[2]]), "model")
  expect_equal(objsz(models[[2]]), 4544)
  expect_equal(class(models[[3]]), "model")
  expect_equal(objsz(models[[3]]), 3192)
  expect_equal(class(models[[4]]), "model")
  expect_equal(objsz(models[[4]]), 3872)
})

test_that("training models works", {

  # Choose directory
  path <- ifelse(dir.exists("test-captchas/"), "test-captchas/",
                 "./tests/testthat/test-captchas/")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)
  files <- files[stringr::str_length(files) > min(stringr::str_length(files))]
  tmp <- tempfile(fileext = ".hdf5")

  # Read captcha
  cap_ans <- read_captcha(files, ans_in_path = TRUE)

  # Train model
  model <- train_model(cap_ans, path = tmp)

  # Expectations
  expect_equal(class(model), "model")
  expect_gt(file.size(tmp), 12000000)
})
