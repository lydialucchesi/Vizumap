#'Build a glyph key
#'
#'This function creates a key of rotated glyphs for a map produced with
#'\code{\link{build_gmap}}.
#'
#'A key for the glyph map is not automatically generated with
#'\code{\link{build_gmap}} and must be made using \code{\link{build_gkey}}. It
#'is important that the arguments passed to \code{\link{build_gkey}} match those
#'passed to \code{\link{build_gmap}}. The map and key can be viewed together
#'using \code{\link{attach_key}}.
#'
#'@param data A data frame.
#'@param glyph Name of glyph shape. Options include \code{icone} and
#'  \code{semi}.
#'@param fontSize An integer value. Default is 3.
#'@param transparent A logical value. Option to make key background transparent.
#'  Default is FALSE.
#'
#'
#'@seealso \code{\link{attach_key}}
#'
#'
#'@examples
#'data(us_data)
#'data(us_geo)
#'co_geo <- subset(us_geo, us_geo@data$STATE == "08")
#'us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
#'co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
#'co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")
#'
#'# build a glyph key
#'key <- build_gkey(data = co_data, glyph = "icone")
#'view(key)
#'
#'@export

build_gkey <- function(data, glyph = "icone", fontSize = 3, transparent = FALSE) {

  nms <- names(data)
  estimate <- nms[1]
  error <- nms[2]

  # run internal function
  glyphDat <- build.glyph.key(shape = glyph)

  # create an empty data frame, which is appended after each loop iteration
  main3 <- data.frame(
    V1 = as.numeric(),
    V2 = as.numeric(),
    id = as.numeric()
  )

  # rotation for 0, middle, and max glyphs on key
  for (i in seq(from = 0,
                to = max(data[ ,error]),
                by = (max(data[ ,error]) / 2))) {
    theta <- (i * -pi) / max(data[ ,error])
    R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
    N <- R %*% t(glyphDat)
    N <- as.data.frame(t(N))
    id <- rep(i, nrow(glyphDat))
    final <- cbind(N, id)
    main3 <- rbind(main3, final)
  }

  # create an empty data frame, which is appended after each loop iteration
  extra5 <- data.frame(
    V1 = as.numeric(),
    V2 = as.numeric(),
    id = as.numeric()
  )

  # rotation for in-between glyphs on key
  for (i in seq(from = 0,
                to = max(data[ ,error]),
                by = (max(data[ ,error]) / 5))) {
    theta <- (i * -pi) / max(data[ ,error])
    R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), nrow = 2)
    N <- R %*% t(glyphDat)
    N <- as.data.frame(t(N))
    id <- rep(i, nrow(glyphDat))
    final <- cbind(N, id)
    extra5 <- rbind(extra5, final)
  }

  # create number labels for 0, middle, and max glyph
  main3_labels <-
    round(seq(
      from = 0,
      to = max(data[ ,error]),
      by = (max(data[ ,error]) / 2)
    ), 2)

  p <- list(main3 = main3, main3_labels = main3_labels, extra5 = extra5, key_label = paste(error), fontSize = fontSize, transparent = transparent)

  oldClass(p) <- c("glyphkey", class(p))

  p

}






