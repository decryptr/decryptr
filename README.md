
decryptr
========

**FOR MORE INFO ABOUT THE OLD API (DECRYPTR LEGACY), SEE [V0.1](https://github.com/decryptr/decryptr/releases/tag/v0.1)**

`decryptr` is an R package to break captchas. It is also an extensible tool built in a way that enables anyone to contribute with their own captcha-breaking code.

Installation
------------

To install `decryptr`, simply run the code below:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("decryptr/decryptr")
```

Basic usage
-----------

`decryptr` has functions for downloading and breaking captchas from multiple known sources. If you wanted to use this package with, let's say, a TRT (Regional Worker's Court), you could go by the following steps:

``` r
# Download captcha from TRT
file <- download_captcha("trt", path = "./img")

# Break captcha
decrypt(file, model = "trt")
```

    ## [1] "k36usu"

Simple, right? The `decrypt()` funcion is this package's workhorse: it is able to take a captcha (either the path to a captcha file or a captcha object read with `read_captcha()`) and break it with a model (either the name of a known model, the path to a model file or a model object created with `train_model()`).

If you'd like to visualize a captcha and make sure the decryption is working, you can use the `plot()` funcion to draw out the captcha image:

``` r
# Read captcha
captcha <- read_captcha(file)

# Plot captcha
plot(captcha)
```

![](README_files/figure-markdown_github/plot-1.png)

If you want to learn more about the models that already come packaged with `decryptr`, check out `load_model()`'s documentation (and all of these models also have a corresponding `download_captcha()` method so you're always good to go).

Advanced usage
--------------

If you're willing to create your own custom captcha-breaking models, there are some other functions you might want to know about. `classify()` allows the user to manually answer a list of captchas, while `train_model()` takes a bunch of classified captchas and trains a `keras` model on them.

`classify()` has two modes: static and interactive. If you already know the answers to all captchas, simply turn them into a string vector and pass it onto the `answers` argument; on the other hand, if you're going to manually classify the captchas, `classify()` will plot every captcha and prompt you in the console for their answers. In the snippet below, I use static classification to label a set of 10 captchas:

``` r
# URL of a captcha (for illustrative purposes I'll be using
# TRT's URL, but you can use whichever URL you want)
url <- "https://pje.trt4.jus.br/consultaprocessual/seam/resource/captcha"

# Download captcha from URL
files <- download_captcha(url, n = 10, path = "./img")

# Answers to downloaded captchas
answers <- c(
  "ew3h3n", "ew3h3n", "da7522", "da7522", "w8kerh",
  "mh52v3", "ny248u", "56nwr5", "7tx6dy", "n3fue6")

# Classify captchas (if answers weren't supplied,
# I'd be promped for interactive classification)
new_files <- classify(files, answers, path = "./img")
```

Now that we have a set of classified captchas, we can use them to train a captcha-breaking model. `classify()` used our answers to create a new version of each file, one with the answer at the end of the filename separated by an underscore; `read_captcha()` has the `ans_in_path` argument that tells it to look for the answers in the filenames and create the captcha objects accordingly.

With this list of labeled captcha objects, we can call `train_model()` to generate a model. The model gets automatically saved to disk so that we can load it later with `load_model()`.

``` r
# Read answered captchas
captchas <- read_captcha(new_files, ans_in_path = TRUE)

# Use captchas to train a model
model <- train_model(captchas, verbose = FALSE)

# Use our new model for decryption
decrypt(file, model = model)
```

    ## [1] "ys3kr4"

``` r
# We could also have loaded the model from disk
model <- load_model("./model.hdf5")
```

Performance
-----------

Once loaded to memory, `keras` models run very quickly Also, we don't run any pre-processing on the image, so decryption is blazing fast.

``` r
microbenchmark::microbenchmark(decrypt = decrypt(captcha, model))
```

    ## Unit: milliseconds
    ##     expr      min       lq    mean   median       uq      max neval
    ##  decrypt 6.739689 7.132197 8.46989 7.375482 7.747622 97.18051   100
