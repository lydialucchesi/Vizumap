

rotateGlyph <- function(x, theta, est, error, id, long, lat, size, glyph) {

  #run internal function
  glyphDat <- build.glyph.map(shape = glyph[x])

  R <- matrix(c(cos(theta[x]), sin(theta[x]), -sin(theta[x]), cos(theta[x])), nrow = 2)

  N <- R %*% t(glyphDat / size[x])
  N <- as.data.frame(t(N))

  val <- rep(est[x], nrow(glyphDat))
  err <- rep(error[x], nrow(glyphDat))
  id <- rep(id[x], nrow(glyphDat))
  previous <- as.data.frame(cbind(val, err, id))

  long <- N$V1 + long[x]
  lat <- N$V2 + lat[x]

  final <- cbind(previous, long, lat)
  final

}


