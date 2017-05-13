
plot.trjs_tibble <- function(x, ...) {
  x <- as_tibble(x)
  x <- dplyr::mutate(x, cor = rgb(r, g, b), id = 1:n())
  p <- ggplot2::ggplot(x, ggplot2::aes(x = x, y = y))
  p <- p +
    ggplot2::coord_equal() +
    ggplot2::theme_bw()
  if(tibble::has_name(x, 'posicao')) {
    p <- p + ggplot2::geom_point(shape = 15)
    p <- p + ggplot2::facet_wrap(~posicao, scales = 'free_x', ncol = 6)
  } else {
    p <- p + ggplot2::geom_point(colour = x$cor, shape = 15, size = 3)
  }
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5)
}
