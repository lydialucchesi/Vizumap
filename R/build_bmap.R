#'Build a bivariate colour map
#'
#'This function builds a map that visualises estimates and errors simultaneously
#'with a bivariate colour scheme.
#'
#'If \code{shapefile} remains \code{NULL}, the function will produce a map of
#'plotted points representing specific sites; in this case, the data frame must
#'include latitude and longitude coordinates in columns \code{"long"} and
#'\code{"lat"}.
#'
#'@param data A data frame.
#'@param shapefile A spatial polygons data frame.
#'@param id Name of the common column shared by the objects passed to
#'  \code{data} and \code{shapefile}. The estimates and errors in the data frame
#'  will be matched to the geographical regions of the spatial polygons data
#'  frame through this column.
#'@param border Name of geographical borders to be added to the map. It must be
#'  one of \code{\link[maps]{county}}, \code{\link[maps]{france}},
#'  \code{\link[maps]{italy}}, \code{\link[maps]{nz}},
#'  \code{\link[maps]{state}}, \code{\link[maps]{usa}} or
#'  \code{\link[maps]{world}} (see documentation for
#'  \code{\link[ggplot2]{map_data}} for more information). The borders will be
#'  refined to match latitute and longtidue coordinates provided in the data
#'  frame or spatial polygons data frame.
#'@param palette Name of colour palette or character vector of hex colour codes
#'  from the \code{\link{build_palette}} function. Colour palette names include
#'  \code{BlueYellow}, \code{CyanMagenta}, \code{BlueRed} and \code{GreenBlue}.
#'@param size An integer between 1 and 20. Value controls the size of points
#'  when \code{shapefile = NULL}. If \code{size = NULL}, the points will remain
#'  the default size.
#'@param terciles A logical value. This provides the option to define numerical
#'  bounds for the colour key grid using terciles instead of equal intervals.
#'@examples
#'data(us_data)
#'data(us_geo)
#'poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")
#'
#'#bivariate map with a shapefile
#'map <- build_bmap(data = poverty, shapefile = us_geo, id = "GEO_ID",
#'  border = "state", terciles = TRUE)
#'view(map)
#'
#'@export
#'@import "ggplot2"
#'@importFrom "dplyr" "left_join"
#'@importFrom "plyr" "join"
#'@importFrom "ggmap" "make_bbox"



build_bmap <- function(data, shapefile = NULL, id = NULL, border = NULL, palette = "BlueYellow", size = NULL,
                      terciles = FALSE){


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

  if (!is.null(border)) {
    m <- (border %in% c("county", "france", "italy", "nz", "state", "usa", "world"))
    if (length(m) == 0)
      stop("Border name not recognised. Must be one of county, france, italy, nz, state, usa or world \n
           (see documentation for map_data function in ggplot2 for more information)")
    else
      bord <- map_data(border)
  }
  else {
    long <- numeric(0)
    lat = numeric(0)
    bord <- data.frame(long = long, lat = lat, group = numeric(0))
  }

  if (!is.null(size)) {
    s <- seq(1, 20, by = 1)
    if (!(size %in% s) & !is.null(size))
      stop("Size not recognised. Must be an integer between 1 and 20.")
  }

  bound <- findNbounds(data = data, estimate = estimate, error = error, terciles = terciles)

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


  #assign each region or point a color based on its estimate and its error
  data$hex_code <-
    ifelse(data[ ,estimate] <= bound[2] & data[ ,error] <= bound[6], colors[1],
           ifelse(data[ ,estimate] <= bound[2] & data[ ,error] <= bound[7], colors[4],
                  ifelse(data[ ,estimate] <= bound[2] & data[ ,error] <= bound[8], colors[7],
                         ifelse(data[ ,estimate] <= bound[3] & data[ ,error] <= bound[6], colors[2],
                                ifelse(data[ ,estimate] <= bound[3] & data[ ,error] <= bound[7], colors[5],
                                       ifelse(data[ ,estimate] <= bound[3] & data[ ,error] <= bound[8], colors[8],
                                              ifelse(data[ ,estimate] <= bound[4] & data[ ,error] <= bound[6], colors[3],
                                                     ifelse(data[ ,estimate] <= bound[4] & data[ ,error] <= bound[7], colors[6],
                                                            ifelse(data[ ,estimate] <= bound[4] & data[ ,error] <= bound[8], colors[9], "NA")))))))))

  #determine whether shapefile has been entered by user
  #if so, link shapefile and data and plot
  if (!is.null(shapefile)) {
    shapefile@data <- left_join(shapefile@data, data, by = id)
    shapefile@data$id <- rownames(shapefile@data)
    region_coord <- tidy(shapefile, region = "id")
    output_data <- join(region_coord, shapefile@data, by = "id")
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)
  }
  else {
    output_data <- data
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)
  }

  if (is.null(size))
    m <- list(output_data = output_data, bord = bord, bbox = bbox)
  else
    m <- list(output_data = output_data, bord = bord, bbox = bbox, size = size)

  oldClass(m) <- c("bivmap", class(m))

  m

}

