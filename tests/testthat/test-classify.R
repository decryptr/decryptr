context("classify")

test_that("classification works", {

  # Choose directory
  path <- ifelse(dir.exists("test-captchas/"), "test-captchas/",
                 "./tests/testthat/test-captchas/")

  # Copy files to temporary dir
  dir <- tempdir()
  file.copy(path, dir, recursive = TRUE)
  dir <- paste0(dir, "/test-captchas")

  # Setup
  files <- list.files(dir, pattern = "^[^_]+$", full.names = TRUE)

  # Classify
  files_ <- classify(files, answers = rep("abcdef", 11), path = dir)

  # Expectations
  expect_length(list.files(dir, pattern = "abcdef", full.names = TRUE), 11)

  # Classify
  files_ <- classify(files, answers = rep("abcdef", 11), path = dir, rm_old = TRUE)

  # Expectations
  expect_length(list.files(dir, pattern = "^[^_]+$", full.names = TRUE), 0)
})
