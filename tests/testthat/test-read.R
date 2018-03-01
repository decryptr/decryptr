context("read")

test_that("reading works", {

  # Choose directory
  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)

  # Read captchas with and without answers
  cap <- read_captcha(files)
  cap_ans <- read_captcha(files, ans_in_path = TRUE)
  one_cap <- cap[[3]]
  one_cap_ans <- cap_ans[[6]]

  # Expectations
  expect_named(one_cap, c("y", "x"))
  expect_match(attr(one_cap, "file"), "\\.jpeg")
  expect_null(one_cap$y)
  expect_equal(dim(one_cap$x), c(35, 120, 1))
  expect_named(one_cap_ans, c("y", "x"))
  expect_match(attr(one_cap_ans, "file"), "\\.jpeg")
  expect_equal(length(one_cap_ans$y), 6)
  expect_equal(dim(one_cap_ans$x), c(35, 120, 1))
})

test_that("reading works with vocabualry", {

  # Choose directory
  path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
                 "./tests/testthat/sample-captchas/")

  # Setup
  files <- list.files(path, pattern = "_", full.names = TRUE)
  vocab <- c(letters, 0:9)

  # Reading
  cap <- read_captcha(files, ans_in_path = TRUE, vocab = vocab)
  one_cap_ans <- cap[[6]]

  # Expectations
  expect_named(one_cap_ans, c("y", "x"))
  expect_match(attr(one_cap_ans, "file"), "\\.jpeg")
  expect_equal(dim(one_cap_ans$y), c(6, length(vocab)))
  expect_equal(dim(one_cap_ans$x), c(35, 120, 1))
  expect_equal(colnames(one_cap_ans$y), vocab)

})
