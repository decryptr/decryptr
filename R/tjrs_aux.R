
print.captcha_tjrs <- function(x, ...) {
  print(as.character(x), ...)
}

print.tjrs_tibble <- function(x, ...) {
  print(as_tibble(x))
}

as_tibble.trjs_tibble <- function(x, ...) {
  class(x) <- "data.frame"
  as_tibble(x)
}
