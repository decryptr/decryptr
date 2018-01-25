context("download")

test_that("downloading works", {

  # Setup
  path <- stringr::str_c(tempdir(), "/NewDir")

  # Download 3 captchas from each source
  files <- c()
  for (url in c("tjrs", "tjmg", "tjrj", "trt", "rfb")) {
     files <- append(files, download_captcha(url, n = 3, path = path, timeout = 10))
  }

  # Expectations
  expect_true(dir.exists(path))
  expect_length(files, 15)
  expect_true(all(file.size(files) > 1000))
})
