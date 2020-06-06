# context("model")
#
# test_that("reading models works", {
#
#   if (!keras:::have_h5py())
#     skip("h5py not available for testing")
#
#   # Setup
#   models <- list(
#     load_model("rfb"),
#     load_model("trt"),
#     load_model("tjmg"),
#     load_model("esaj"))
#
#   # Calculate object size
#   objsz <- function(x) { as.integer(object.size(x)) }
#
#   # Expectations
#   expect_equal(class(models[[1]]), "model")
#   expect_gt(objsz(models[[1]]), 2000)
#   expect_equal(class(models[[2]]), "model")
#   expect_gt(objsz(models[[2]]), 2000)
#   expect_equal(class(models[[3]]), "model")
#   expect_gt(objsz(models[[3]]), 2000)
#   expect_equal(class(models[[4]]), "model")
#   expect_gt(objsz(models[[4]]), 2000)
# })
#
# test_that("training models works", {
#
#   if (!keras:::have_h5py())
#     skip("h5py not available for testing")
#
#   # Choose directory
#   path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
#                  "./tests/testthat/sample-captchas/")
#   path2 <- ifelse(dir.exists("sample-captchas/"), "./sample-model.hdf5",
#                  "./tests/testthat/sample-model.hdf5")
#
#   # Setup
#   files <- list.files(path, pattern = "_", full.names = TRUE)
#   files <- files[stringr::str_length(files) > min(stringr::str_length(files))]
#
#   # Read captcha
#   cap_ans <- read_captcha(files, ans_in_path = TRUE)
#
#   # Train model
#   model <- train_model(cap_ans, path = path2)
#
#   # Expectations
#   expect_equal(class(model), "model")
#   expect_gt(file.size(path2), 12000000)
# })
#
# test_that("join_captchas works", {
#
#   # Choose directory
#   path <- ifelse(dir.exists("sample-captchas/"), "sample-captchas/",
#                  "./tests/testthat/sample-captchas/")
#
#   # Setup
#   files <- list.files(path, pattern = "_", full.names = TRUE)
#   vocab <- c(letters, 0:9)
#
#   # Reading
#   cap <- read_captcha(files, ans_in_path = TRUE, vocab = vocab)
#   cap_j <- join_captchas(cap)
#
#   cap2 <- read_captcha(files, ans_in_path = TRUE)
#   cap_j2 <- join_captchas(cap2)
#
#   # Expectations
#   expect_equal(dim(cap_j$x), c(10, 35, 120, 1))
#   expect_equal(dim(cap_j$y), c(10, 6, 36))
#   expect_equal(cap_j$n, 10)
#
#   expect_equal(dim(cap_j2$x), c(10, 35, 120, 1))
#   expect_equal(dim(cap_j2$y), c(10, 6, 24))
#   expect_equal(cap_j2$n, 10)
#
#
# })
#
