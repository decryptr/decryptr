context("download")

test_that("downloading works", {

  # Setup
  path <- stringr::str_c(tempdir(), "/NewDir")

  # Download captchas from source
  files <- download_captcha("rfb", n = 6, path = path)

  # Expectations
  expect_true(dir.exists(path))
  expect_length(files, 6)
  expect_true(all(file.size(files) > 1000))
})
