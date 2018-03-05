context("decrypt")

test_that("decryption works", {

  if (!keras:::have_h5py())
    skip("h5py not available for testing")

  # Choose directory
  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")
  path2 <- ifelse(dir.exists("sample-captchas/"), "./sample-model.hdf5",
                  "./tests/testthat/sample-model.hdf5")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)
  files <- files[stringr::str_length(files) > min(stringr::str_length(files))]

  # Read capthcas
  cap <- read_captcha(files)

  # Expectations
  expect_equal(class(decrypt(files, load_model(path2))), "character")
  expect_equal(class(decrypt(files, path2)), "character")
  expect_equal(class(decrypt(files, "trt")), "character")
  expect_equal(class(decrypt(cap, load_model(path2))), "character")
  expect_equal(class(decrypt(cap, path2)), "character")
  expect_equal(class(decrypt(cap, "trt")), "character")
})

test_that("decrypt works with raw vector", {

  if (!keras:::have_h5py())
    skip("h5py not available for testing")

  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")

  captcha_raw <- readr::read_file_raw(paste0(path, "captcha2bce28f1401c_da3vh3.jpeg"))

  expect_equal(class(decrypt(captcha_raw, "trt")), "character")
})
