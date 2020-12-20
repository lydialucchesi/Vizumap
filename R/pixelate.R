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
#'  \code{\link[rgdal]{readOGR}} for more information).
#'@param pixelSize An integer 1, 2 or 3. One corresponds to the smallest pixel
#'  size, and three corresponds to the largest.
#'@param id A name which will be given to the new ID column. This ID corresponds to the slot ID in
#'  the spatial data.
#'
#'@examples
#'data(us_geo)
#'ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
#'pix <- pixelate(ca_geo, id = "region")
#'
#'
#'@importFrom "rgdal" "readOGR"
#'@importFrom "sp" "SpatialPolygons" "spTransform" "proj4string" "CRS" "proj4string<-"
#'@importFrom "rgeos" "readWKT" "gBuffer"
#'@importFrom "ggmap" "make_bbox"
#'@importFrom "geoaxe" "chop"
#'@importFrom "broom" "tidy"
#'@export



pixelate <-
  function(geoData = NULL,
           file = NULL,
           layer = NULL,
           pixelSize = 2,
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

    # find projection of geo object
    if (!is.null(geoData))
      x <- proj4string(geoData)
    else {
      geoData <- readOGR(file, layer = layer)
      x <- proj4string(geoData)
    }

    if (!is.na(x)) {
      if (!is.null(file)) {
        geoData <-
          SpatialPolygons(geoData@polygons, proj4string = geoData@proj4string)
      }
      if (class(geoData) == "SpatialPolygonsDataFrame") {
        geoData <-
          SpatialPolygons(geoData@polygons, proj4string = geoData@proj4string)
      }
    }

    # remove projection
    pol <- "POLYGON((0 0, 0 1, 1 1, 1 0, 0 0))"
    poly <- readWKT(pol)
    geoDataProjNA <- geoData
    proj4string(geoDataProjNA) <- proj4string(poly)

    # create definitive space
    geoDataProjNA <- gBuffer(geoDataProjNA, byid = TRUE, width = 0)

    # prep work for determining values passed to size and n in chop function
    s <- tidy(geoDataProjNA)

    lat <- s$lat
    long <- s$long
    bbox <- make_bbox(lat = lat, lon = long, data = s)
    dif <- c(abs(bbox[1] - bbox[3]), abs(bbox[2] - bbox[4]))
    v <- max(dif)

    # chop into pixels
    if (pixelSize == 1)
      n <- (233 + (1 / 3))
    else if (pixelSize == 2)
      n <- 175
    else if (pixelSize == 3)
      n <- 140
    else
      stop("PixelSize must be one of 1, 2 or 3.\n")

    # calculate pixel size
    size <- v / n

    # pixelate SpatialPolygons object with chop function
    pixel_geoData <- geoaxe::chop(geoDataProjNA, size = size, n = n)
    # return to original projection
    pixel_geoData <-
      SpatialPolygons(pixel_geoData@polygons, proj4string = geoData@proj4string)

    # turn SpatialPolygons object into a data frame of coordinates
    pixel_df <- tidy(pixel_geoData)

    # recreate the slot id of the polygons in a new id column with name provided by
    # user - ideally, for ease, the original slot ids of the polygons should match
    # the id column in their data frame of data or relative frequencies
    # pixel_df[ ,id] <- gsub("\\sg\\d*.\\d", "", pixel_df[ ,"id"]) # OLD CODE
    pixel_df[, id] <-
      as.vector(sapply(pixel_df[, "id"], function(x)
        gsub("\\sg\\d*.\\d", "", x)))

    # create a second ID column that is used in the loops in build_pmap
    # pixel_df$ID <- cumsum(!duplicated(pixel_df[ ,id])) # OLD code
    pixel_df$ID <- cumsum(!duplicated(pixel_df[, "id"]))

    # define specific class to control what maps are used with build_pmap
    oldClass(pixel_df) <- c("pixel_df", class(pixel_df))



    pixel_df

  }
