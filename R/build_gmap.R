#'Build a glyph map
#'
#'This function builds a map that visualises estimates and errors simultaneously
#'with rotated glyphs.
#'
#'If a shapefile is used, glyphs will be plotted at region
#'centroids. If \code{shapefile} remains \code{NULL}, glyphs will be plotted at
#'points on the map representing specific sites; in this case, the data frame
#'must include latitude and longitude coordinates in columns \code{"long"} and
#'\code{"lat"}.
#'
#'@param data A data frame.
#'@param shapefile A spatial polygons data frame.
#'@param id Name of the common column shared by the objects passed to
#'  \code{data} and \code{shapefile}. The estimates and errors in the data frame
#'  will be matched to the geographical regions of the spatial polygons data
#'  frame through this column.
#'@param size An integer between 1 and 100. Value controls the size of the glyphs.
#'@param border Name of geographical borders to be added to the map. It must be
#'  one of \code{\link[maps]{county}}, \code{\link[maps]{france}},
#'  \code{\link[maps]{italy}}, \code{\link[maps]{nz}},
#'  \code{\link[maps]{state}}, \code{\link[maps]{usa}} or
#'  \code{\link[maps]{world}} (see documentation for
#'  \code{\link[ggplot2]{map_data}} for more information). The borders will be
#'  refined to match latitute and longtidue coordinates provided in the data
#'  frame or spatial polygons data frame.
#'@param glyph Name of glyph shape. Options include \code{icone} and \code{semi}.
#'@param palette Name of colour palette. It must be one of \code{Blues},
#'  \code{Greens}, \code{Greys}, \code{Oranges}, \code{Purples} or \code{Reds}
#'  (see documentation for \code{\link[ggplot2]{scale_fill_distiller}} for more
#'  information).
#'@examples
#'data(us_data)
#'data(us_geo)
#'co_geo <- subset(us_geo, us_geo@data$STATE == "08")
#'us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
#'co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
#'co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")
#'
#'#build a glyph map
#'map <- build_gmap(data = co_data, shapefile = co_geo, id = "GEO_ID",
#'  size = 70, border = "state", glyph = "icone")
#'view(map)
#'
#'@export
#'@importFrom "sp" "coordinates"
#'@importFrom "ggmap" "make_bbox"
#'@importFrom "maptools" "map2SpatialPolygons"


build_gmap <- function(data, shapefile = NULL, id = NULL, size = 50, border = NULL, glyph = "icone", palette = "Blues") {

  nms <- names(data)
  estimate <- nms[1]
  error <- nms[2]

  if (is.null(shapefile)) {
    l1 <- match("long", names(data))
    l2 <- match("lat", names(data))
    if (any(is.na(c(l1, l2))))
      stop("There must be coordinates in data columns named long and lat.\n")
  }

  if (!is.null(shapefile) & is.null(id)) {
    stop("Missing id. Must be a common column shared by data and shapefile.")
  }

  if (is.null(shapefile)) {
    df <- data
  } else {
    centroids <- as.data.frame(coordinates(shapefile))
    names(centroids) <- c("long", "lat")
    shapefile@data <- cbind(shapefile@data, centroids)
    shapefile@data[, estimate] <-
      data[match(shapefile@data[, id], data[, id]), 1]
    shapefile@data[, error] <-
      data[match(shapefile@data[, id], data[, id]), 2]
    df <- shapefile@data
  }

  if (!is.null(border)) {
    if (border %in% c("county", "france", "italy", "nz", "state", "usa", "world"))
      bord <- map_data(border)
    else
      stop("Border name not recognised. Must be one of county, france, italy, nz, state, usa or world \n
           (see documentation for map_data function in ggplot2 for more information)")
  }
  else {
    long <- numeric(0)
    lat = numeric(0)
    bord <- data.frame(long = long, lat = lat, group = numeric(0))
  }

  s <- seq(1, 100, by = 1)
  if (!(size %in% s))
    stop("Size not recognised. Must be an integer between 1 and 100.")
  else
    size = 101 - size

  if (!(palette %in% c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds")))
    stop("Palette name not recognised. Must be one of Blues, Greens, Greys, Oranges, Purples or Reds \n
         (see documentation for scale_fill_distiller in ggplot2 for more information)")

  #calculate theta for use in rotation matrix
  df$theta <- (df[, error] / max(df[, error])) * (-pi)



  #create id for use in loop
  df$id <- seq(from = 1,
               to = nrow(df),
               by = 1)

  df$size <- rep(size, nrow(df))

  df$glyph <- rep(glyph, nrow(df))

  #create an empty data frame, which is appended after each loop iteration
  array <- tapply(1:nrow(df), df$id,

                  function(x, theta, est, error, id, long, lat, size, glyph)
                    rotateGlyph(x, theta, est, error, id, long, lat, size, glyph),

                  theta = df$theta,
                  est = df[,estimate],
                  error = df[,error],
                  id = df$id,
                  long = df$long,
                  lat = df$lat,
                  size = df$size,
                  glyph = df$glyph)

  output_data <- do.call(rbind.data.frame, array)

  output_data$val <- as.numeric(as.character(output_data$val))
  output_data$err <- as.numeric(as.character(output_data$err))

  bbox <- make_bbox(lat = lat, lon = long, data = output_data)

  p <- list(output_data = output_data, bord = bord, bbox = bbox, key_label = paste(estimate), palette = palette)

  oldClass(p) <- c("glyphmap", class(p))

  p

}
