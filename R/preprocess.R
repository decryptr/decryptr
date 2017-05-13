preprocess_tjrs <- function(object, nm) {
  # Preencher
  preencher <- function(img, x, y, lim, x_min = 0, y_min = 0) {
    img_completa = expand.grid(x = x_min:x, y = y_min:y)
    dplyr::left_join(img_completa, img, c('x', 'y')) %>%
      dplyr::mutate(
        r = ifelse(is.na(r), 1, r),
        r = ifelse(r > lim, 1, 0)
      )
  }

  # Converter em matriz
  converter_em_matriz <- function(img){
    img %>%
      dplyr::select(x, y, r) %>%
      tidyr::spread(x, r, fill = 1) %>%
      dplyr::select(-y) %>%
      as.matrix()
  }

  # Converter em data.frame
  converter_em_df <- function(m) {
    as.data.frame(m) %>%
      dplyr::mutate(y = as.numeric(1:nrow(.))) %>%
      tidyr::gather(x, r, -y) %>%
      dplyr::mutate(x = readr::parse_number(x))
  }

  # Limpar imagem
  limpar <- function(img, n = 1, k = 6, x = 170, y = 30, lim = 0){

    fk <- function(x) {sum(x == 0)}
    mk <- matrix(1, 1 + 2 * n, 1 + 2 * n)

    arrumado <- preencher(img, x, y, lim)

    for(j in k) {
      m_inicial <- arrumado %>% converter_em_matriz()
      m <- m_inicial %>%
        raster::raster() %>%
        raster::focal(mk, fk, pad = TRUE, padValue = 1) %>%
        raster::as.matrix()
      m <- ifelse(m >= j & m_inicial == 0, 0, 1)
      arrumado <- converter_em_df(m)
    }

    arrumado %>% dplyr::filter(r == 0)
  }

  ler_tibble <- function(a) {
    img <- jpeg::readJPEG(a)
    img_dim <- dim(img)
    img_df <- data.frame(
      x = rep(1:img_dim[2], each = img_dim[1]),
      y = rep(img_dim[1]:1, img_dim[2]),
      r = as.vector(img[,,1]),
      g = as.vector(img[,,2]),
      b = as.vector(img[,,3])
    )
    d <- dplyr::mutate(img_df, cor = grDevices::rgb(r, g, b), id = 1:n())
    d <- dplyr::filter(d, cor != '#FFFFFF')
    tibble::as_data_frame(d)
  }

  # Arruma o arquivo num BD pronto para predição.
  arrumar <- function(arqs_treino, nm) {
    # nm <- deparse(substitute(...))
    bd <- tibble::data_frame(arq = arqs_treino) %>%
      dplyr::arrange(arq) %>%
      dplyr::mutate(.id = 1:n()) %>%
      dplyr::group_by(arq) %>%
      dplyr::do({
        ler_tibble(.$arq) %>%
          dplyr::mutate(r = g, b = g) %>%
          dplyr::mutate(r = 1 - r) %>%
          dplyr::filter(r <= .2) %>%
          dplyr::mutate(r = ifelse(r < 1, 0, 1)) %>%
          dplyr::mutate(g = r, b = r) %>%
          limpar(y = 40, k = 6) %>%
          dplyr::mutate(g = r, b = r) %>%
          dplyr::filter(y >= 5, y <= 40, x >= 10, x <= 106) %>%
          dplyr::mutate(group = cut(x, c(10, 34, 58, 82, 106), labels = c(1, 2, 3, 4)),
                        group = as.character(group),
                        group = ifelse(is.na(group), '4', group))
      }) %>%
      dplyr::ungroup()
    d <- bd %>%
      dplyr::mutate(letras = stringr::str_match(basename(arq), '([0-9]{4})_')[, 2]) %>%
      dplyr::group_by(arq) %>%
      dplyr::mutate(letra = stringr::str_sub(letras, as.numeric(group), as.numeric(group))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(x = x - 24 * (as.numeric(group) - 1)) %>%
      dplyr::select(arq, letra, group, x, y, r) %>%
      dplyr::mutate(x = sprintf('%03d', x), y = sprintf('%03d', y)) %>%
      tidyr::unite(xy, x, y, sep = '_') %>%
      tidyr::spread(xy, r, fill = 1)
    nm <- nm[nm != '.outcome']
    novos <- nm[!nm %in% names(d)]
    d[, novos] <- 1
    d
  }
  arrumar(object, nm)
}


