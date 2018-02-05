context("print")

test_that("printing works", {
  # Choose directory
  path <- ifelse(dir.exists("test-captchas/"), "test-captchas/",
                 "./tests/testthat/test-captchas/")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)

  # Read objects we want to print
  cap <- read_captcha(files)

  # Expectations
  expect_output(print(cap), "A list of 10.+01\\. \\\".*test-captchas//captcha")
  expect_output(print(cap[[1]]), "A captcha.+\\\".*test-captchas//captcha")
  expect_equal(class(plot(cap[[2]])), "raster")
})
