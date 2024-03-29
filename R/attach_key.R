#'Attach a key
#'
#'This function attaches a bivariate colour key to a
#'bivariate colour map or a glyph key to a glyph map so that they can be viewed together.
#'
#' @param map An object generated with \code{\link{build_bmap}} or \code{\link{build_gmap}}.
#' @param mapkey An object generated with \code{\link{build_bkey}} or \code{\link{build_gkey}}.
#' @examples
#'data(us_data)
#'data(us_geo)
#'poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")
#'
#'# bivariate map and key together
#'map <- build_bmap(data = poverty, geoData = us_geo, id = "GEO_ID",
#'  border = "state", terciles = TRUE)
#'key <- build_bkey(data = poverty, terciles = TRUE)
#'attach_key(map, key)
#' @export
#' @import "patchwork"

attach_key <- function(map, mapkey) {

  if(!(class(mapkey)[1] %in% c("bivkey", "glyphkey")))
    stop("Object mapkey is not of class 'bivkey' or 'glyphkey'")

  if(!(class(map)[1] %in% c("bivmap", "glyphmap")))
    stop("Object map is not of class 'bivmap' or 'glyphmap'")

  k <- view(mapkey)
  m <- view(map)

  m + k + plot_layout(widths = c(4, 1), heights = c(4, 1))


}

