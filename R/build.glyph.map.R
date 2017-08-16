#internal function to build glyph for maps

build.glyph.map <- function(shape) {

  #right now there are only two glyphs, but we can add to this
  g <- c("icone", "semi")
  if ((shape %in% g) == FALSE) {
    stop("Glyph name not recognised. Must be one of icone or semi.")
  }

  #create data frame of coordinates for glyph for key
  if (shape == "icone") {
    x <- seq(-3, 3, .05)
    y = sqrt(9 - x ^ 2)
    cir <- as.data.frame(cbind(x, y))
    x <- c(-3, 0, 3)
    y <- c(0,-5, 0)
    tri <- as.data.frame(cbind(x, y))
    glyphDat <- rbind(cir, tri)
  } else {
    x <- seq(-3, 3, .05)
    y = sqrt(9 - x ^ 2)
    glyphDat <- as.data.frame(cbind(x, y))
  }

  #output glyphDat for use in the build_glyphMap function
  glyphDat

}
