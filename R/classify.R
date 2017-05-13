
classify <- function(x, ...) {
  UseMethod("classify")
}

classify.tjrs_file <- function(x, path, answer = NULL) {
  suppressWarnings(dir.create(path))
  print(plot(read(x)))
  if(is.null(answer)) {answer <- readline(prompt="Answer: ") }
  file.copy(x, sprintf('%s/%s.jpeg', path, answer))
}
