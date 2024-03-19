#'Pixelate polygons
#'
#'This function divides a shapefile into pixels so it can be used to create a
#'pixel map with \code{\link{build_pmap}}.
#'
#'This function can take several minutes to run depending on the size of the shapefile. Within this function, the projection of the spatial object is removed and then returned to the original projection.
#'
#'@param geoData An object of class "SpatialPolygons" or
#'  "SpatialPolygonsDataFrame".
#'@param file A shapefile pathway.
#'@param layer Name of geoData layer (see documentation for
#'  \code{\link[sf]{read_sf}} for more information).
#'@param pixelSize An integer. Larger values result in smaller pixels.
#'@param id The ID column shared by the geoData object and the dataset with
#'  estimates and errors.
#'
#'@examples
#'data(us_geo)
#'ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
#'pix <- pixelate(ca_geo, pixelSize = 60, id = "GEO_ID")
#'
#'@import sf
#'@importFrom "FRK" "SpatialPolygonsDataFrame_to_df"
#'@export

pixelate <-
  function(geoData = NULL,
           file = NULL,
           layer = NULL,
           pixelSize = 40,
           id = "id") {
    # check for geoData or file
    if (is.null(geoData) & is.null(file))
      stop("One of geoData or file needs to be supplied to this function.\n")

    # check class of geoData
    if (is.null(file) &
        (!(
          class(geoData) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")
        )))
      stop("geoData must be one of class SpatialPolygons or SpatialPolygonsDataFrame.\n")

    # check that layer is entered with file
    if (!is.null(file) & is.null(layer))
      stop("Layer needs to be supplied if file is supplied.\n")

    # create a grid of pixels that covers the entire map
    full_grid <- st_make_grid(geoData, n = pixelSize)

    # define a function that finds the pixels inside each region
    pixel_poly <- function(x) {
      grid <- st_intersection(full_grid, st_as_sf(geoData[x,]))
      grid <- st_cast(grid, "POLYGON")
      grid <- as_Spatial(grid)
      grid$ID <- rep(geoData[x, id][[1]], length(grid))
      return(grid)
    }

    # reformat all of the region grids into a single data frame
    list_grids <- lapply(1:length(geoData), pixel_poly)
    all_grids <- do.call(rbind, list_grids)
    pixel_df <- SpatialPolygonsDataFrame_to_df(all_grids)
    pixel_df$`id.1` <- NULL
    colnames(pixel_df) <- c("long", "lat", "group", id)

    # create a second ID column that is used in the loops in build_pmap
    pixel_df$ID <- cumsum(!duplicated(pixel_df[, id]))

    # define specific class to control what maps are used with build_pmap
    oldClass(pixel_df) <- c("pixel_df", class(pixel_df))

    pixel_df

  }
