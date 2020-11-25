context("decrypt")

test_that("decryption works", {

  # Choose directory
  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")
  path2 <- ifelse(dir.exists("sample-captchas/"), "./sample-model.hdf5",
                  "./tests/testthat/sample-model.hdf5")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)

  # Load model
  model <- load_model(path2)

  # Read capthcas
  cap <- read_captcha(files)

  # Expectations
  expect_equal(class(decrypt(files, model)), "character")
  expect_equal(class(decrypt(files, path2)), "character")
  expect_equal(class(decrypt(files, "rfb")), "character")
  expect_equal(class(decrypt(cap, model)), "character")
  expect_equal(class(decrypt(cap, path2)), "character")
  expect_equal(class(decrypt(cap, "rfb")), "character")
})

test_that("decrypt works with raw vector", {

  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")

  captcha_raw <- readr::read_file_raw(paste0(path, "captcha372a5114848de_f9ccnk.png"))

  expect_equal(class(decrypt(captcha_raw, "rfb")), "character")
})
