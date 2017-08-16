#'Build a bivariate colour key
#'
#'This function links data and a colour palette. Numerical bounds are added
#'to the 3 x 3 colour grid.
#'
#'A key for the bivariate map is not automatically generated with
#'\code{\link{build_bmap}} and must be made using \code{\link{build_bkey}}. It is important
#'that the arguments passed to \code{\link{build_bkey}} match those passed to
#'\code{\link{build_bmap}}. The map and key can be viewed together using
#'\code{\link{attach_key}}.
#'
#'@param data A data frame.
#'@param palette Name of colour palette or character vector of hex colour codes
#'  created with \code{\link{build_palette}} function. Colour palette names include
#'  \code{BlueYellow}, \code{CyanMagenta}, \code{BlueRed} and
#'  \code{GreenBlue}.
#'@param terciles A logical value. This provides the option to define numerical
#'  bounds for the colour key grid using terciles instead of equal intervals.
#'@examples
#'data(us_data)
#'data(us_geo)
#'poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")
#'
#'#use a prepared palette and terciles
#'key <- build_bkey(data = poverty, terciles = TRUE)
#'view(key)
#'
#'#use a created palette
#'p <- build_palette(name = "usr", colrange =
#'  list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))
#'key <- build_bkey(data = poverty, palette = p)
#'view(key)
#'@export


build_bkey <- function(data, palette = "BlueYellow", terciles = FALSE) {


  estimate <- names(data)[1]
  error <- names(data)[2]


  bound <- findNbounds(data = data, estimate = estimate, error = error, terciles = terciles)

  labels <- data.frame(x = c(2.5, 1.5, .5, -.5, 3.5, 4.5, 5.5, 6.5),
                       y = c(-.5, .5, 1.5, 2.5, -.5, .5, 1.5, 2.5),
                       bound = as.character(bound), angle = c(rep(45,4), rep(-45, 4)))

  #define color ramps based on user input
  if (class(palette)[1] == "character" & length(palette)==1) {
    if (palette == "BlueYellow")
      colors <- build_palette(name = "BlueYellow")
   else if (palette == "CyanMagenta")
     colors <- build_palette(name = "CyanMagenta")
    else if (palette == "BlueRed")
      colors <- build_palette(name = "BlueRed")
    else if (palette == "GreenBlue")
      colors <- build_palette(name = "GreenBlue")
    else
      stop("Palette name not recognised. Must be one of BlueYellow, CyanMagenta, BlueRed or GreenBlue.\n")
  }
  else if (class(palette)[1] == "palette")
    colors <- palette  # assign a users palette creation
  else
    stop("Palette supplied is not of class 'palette'. Please create a palette using the 'build_palette' function.")

  x1 <- c(3, 4, 3, 2) ; x2 <- c(4, 5, 4, 3) ; x3 <- c(5, 6, 5, 4)
  x <- c(x1, x1-1, x1-2, x2, x2-1, x2-2, x3, x3-1, x3-2)

  y1 <- c(0, 1, 2, 1) ; y2 <- c(1, 2, 3, 2) ; y3 <- c(2, 3, 4, 3)
  y <- c(y1, y1+1, y1+2, y2, y2+1, y2+2, y3, y3+1, y3+2)

  group <- rep(1:9, each = 4)

  clr <- rep(colors, each = 4)

  tiles <- data.frame(x = x, y = y, group = group, color = clr)



  p <- list(tiles = tiles, labels = labels, estimate = estimate, error = error)

  oldClass(p) <- c("bivkey", class(p))

  p

}


