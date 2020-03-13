#'Build an exceedance probability colour map
#'
#'This function builds a map that visualises the probability of exceeding some
#'threshold of concern.
#'
#'If \code{shapefile} remains \code{NULL}, the function will produce a map of
#'plotted points representing specific sites; in this case, the data frame must
#'include latitude and longitude coordinates in columns \code{"long"} and
#'\code{"lat"}.
#'
#'@param data A data frame.
#'@param shapefile A spatial polygons data frame.
#'@param id Name of the common column shared by the objects passed to
#'  \code{data} and \code{shapefile}. The exceedance probability in the data frame
#'  will be matched to the geographical regions of the spatial polygons data
#'  frame through this column.
#'@param key_label Label of legend.
#'@param border Name of geographical borders to be added to the map. It must be
#'  one of \code{\link[maps]{county}}, \code{\link[maps]{france}},
#'  \code{\link[maps]{italy}}, \code{\link[maps]{nz}},
#'  \code{\link[maps]{state}}, \code{\link[maps]{usa}} or
#'  \code{\link[maps]{world}} (see documentation for
#'  \code{\link[ggplot2]{map_data}} for more information). The borders will be
#'  refined to match latitute and longtidue coordinates provided in the data
#'  frame or spatial polygons data frame.
#'@param palette Name of colour palette. Colour palette names include
#'  \code{BlueYellow}, \code{CyanMagenta}, \code{BlueRed}, \code{GreenBlue} and \code{YellowRed}.
#'@param size An integer between 1 and 20. Value controls the size of points
#'  when \code{shapefile = NULL}. If \code{size = NULL}, the points will remain
#'  the default size.
#'
#'
#'@examples
#'
#'
#'@import "ggplot2"
#'@importFrom "dplyr" "left_join"
#'@importFrom "plyr" "join"
#'@importFrom "dplyr" "rename"
#'@importFrom "dplyr" "%>%"
#'@importFrom "ggmap" "make_bbox"
#'@importFrom "spbabel" "sptable"
#'@export



build_emap <- function(data, shapefile = NULL, id = NULL, key_label,
                       palette = "YlOrRd", size = NULL, border = NULL) {


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


  #check palette name
  if (!(palette %in% c("YlOrBr", "YlOrRd", "YlGnBu", "PuBuGn")))
    stop("Palette name not recognised. Must be one of YlOrBr, YlOrRd, YlGnBu or PuBuGn, \n
         (see documentation for scale_fill_distiller in ggplot2 for more information).\n")


  #determine whether shapefile has been entered by user
  #if so, link shapefile and data and plot
  if (!is.null(shapefile)) {
    shapefile@data <- shapefile@data %>% dplyr::mutate_if(is.factor,
                                                          as.character)
    shapefile@data <- left_join(shapefile@data, data, by = "scID")
    shapefile@data$id <- rownames(shapefile@data)
    region_coord <- sptable(shapefile, region = "id")
    region_coord <- plyr::rename(region_coord, c(object_ = "id",
                                                 x_ = "long", y_ = "lat", branch_ = "group"))
    output_data <- plyr::join(region_coord, shapefile@data, by = "id")
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)

  }
  else {
    output_data <- data
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)
  }

  id <- match(names(data)[1], names(output_data))
  names(output_data)[id] <- "Exceedance"

  p <- list(output_data = output_data, bord = bord, bbox = bbox,
            key_label = key_label, palette = palette)

  oldClass(p) <- c("emap", class(p))

  p

}
