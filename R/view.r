#'View a \strong{Vizumap} object
#'
#'This function views maps, keys, palettes and animations created with
#'\strong{Vizumap}.
#'
#'For maps, keys and palettes, assigning the output from \code{\link{view}} to a variable
#'name saves the output as an object of class \code{"ggplot"}. Animations
#'created with \code{\link{animate}} are viewed through an internet browser
#'as html objects.
#'
#'@param obj An object from \code{\link{build_bmap}}, \code{\link{build_gmap}},
#'  \code{\link{build_pmap}}, \code{\link{build_bkey}},
#'  \code{\link{build_gkey}}, \code{\link{build_palette}}, or
#'  \code{\link{animate}}.
#'@examples
#'gb <- build_palette(name = "GreenBlue")
#'view(gb)
#'
#'# ggplot object
#'p <- view(gb)
#'
#'
#'@export


view <- function(obj)
  UseMethod("view")

