# decryptr

### Description

`decryptr` is an R package for breaking captchas. It is also an extensible tool
built in a way that enables anyone to contribute with their own captcha-breaking
code.

To install `decryptr`, simply run the code below:

```r
install.packages("devtools")
devtools::install_github("decryptr/decryptr")
```

### Basic usage

`decryptr` has functions for downloading and breaking captchas from multiple
sources, one of which is TJRS (a court in southern Brazil). Here is how one
could break captchas from TJRS:

```r
# Download captchas
x <- donwload_tjrs()

# Classify captchas manually to train a model
classify(x, "~/captchas")

# Break captchas
donwload_tjrs() %T>% plot() %>% predict()
```

### Extensibility

Since `decryptr` is built on top of S3 methods, anyone can extend its functionalities
with custom packages. Simply create a `download_<ext>` function, a `read_<ext>`
function, and a `predict.<ext>` function and you're good to go.

More information about extensibility can be found on `decryptr`'s documentation.
