#'Pixelate polygons
#'
#'This function divides a shapefile into pixels so it can be used to create a
#'pixel map with \code{\link{build_pmap}}.
#'
#'This function can take several minutes to run depending on the size of the shapefile.
#'
#'@param shapefile An object of class "SpatialPolygons" or
#'  "SpatialPolygonsDataFrame".
#'@param file A shapefile pathway.
#'@param layer Name of shapefile layer (see documentation for
#'  \code{\link[rgdal]{readOGR}} for more information).
#'@param pixelSize An integer 1, 2 or 3. One corresponds to the smallest pixel
#'  size, and three corresponds to the largest.
#'@param id A name which will be given to the new ID column. This ID corresponds to the slot ID in
#'  the shapefile.
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



pixelate <- function(shapefile = NULL, file = NULL, layer = NULL, pixelSize = 2, id = "id") {



  #check for shapefile or file
  if (is.null(shapefile) & is.null(file))
    stop("One of shapefile or file needs to be supplied to this function.\n")

  #check class of shapefile
  if (is.null(file) & (!(class(shapefile) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame"))))
    stop("Shapefile must be one of class SpatialPolygons or SpatialPolygonsDataFrame.\n")

  #check that layer is entered with file
  if (!is.null(file) & is.null(layer))
    stop("Layer needs to be supplied if file is supplied.\n")

  #find projection of geo object
  if (!is.null(shapefile))
    x <- proj4string(shapefile)
  else {
    shapefile <- readOGR(file, layer = layer)
    x <- proj4string(shapefile)
  }

  #change projection to "+proj=longlat +datum=WGS84" if not NA, this proj
  #works with chop (e.g. with this proj (which is the proj supplied when
  #importing a shapefile from factfinder) "+proj=merc +a=6378137 +b=6378137
  #+lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null
  #+no_defs" chop returns an empty SpatialPolygons object)
  if (!is.na(x)) {
    if (!is.null(file)) {
      shapefile <- SpatialPolygons(shapefile@polygons, proj4string = shapefile@proj4string)
      shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
    } else if (class(shapefile) == "SpatialPolygonsDataFrame") {
      shapefile <- SpatialPolygons(shapefile@polygons, proj4string = shapefile@proj4string)
      shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
    }
    else
      shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
  }


  #remove projection
  pol <- "POLYGON((0 0, 0 1, 1 1, 1 0, 0 0))"
  poly <- readWKT(pol)
  proj4string(shapefile) <- proj4string(poly)

  #create definitive space
  shapefile <- gBuffer(shapefile, byid = TRUE, width = 0)

  # suppress warnings that occur with the broom::tidy function as it coerces a factor to character
  # this is OK but warnings are annoying
  oldw <- getOption("warn")
  options(warn = -1)

  #prep work for determining values passed to size and n in chop function
  s <- tidy(shapefile)

  lat <- s$lat
  long <- s$long
  bbox <- make_bbox(lat = lat, lon = long, data = s)
  dif <- c(abs(bbox[1] - bbox[3]), abs(bbox[2] - bbox[4]))
  v <- max(dif)

  #chop into pixels
  if (pixelSize == 1)
    n <- (233 + (1/3))
  else if (pixelSize == 2)
    n <- 175
  else if (pixelSize == 3)
    n <- 140
  else
    stop("PixelSize must be one of 1, 2 or 3.\n")

  #calculate pixel size
  size <- v/n

  #pixelate SpatialPolygons object with chop function
  pixel_shapefile <- chop(shapefile, size = size, n = n)

  #turn SpatialPolygons object into a data frame of coordinates
  pixel_df <- tidy(pixel_shapefile)

  options(warn = oldw)

  #recreate the slot id of the polygons in a new id column with name provided by
  #user - ideally, for ease, the original slot ids of the polygons should match
  #the id column in their data frame of data or relative frequencies
  # pixel_df[ ,id] <- gsub("\\sg\\d*.\\d", "", pixel_df[ ,"id"]) # OLD CODE
  pixel_df[ ,id] <- as.vector(sapply(pixel_df[,"id"], function(x) gsub("\\sg\\d*.\\d", "", x)))

  #create a second ID column that is used in the loops in build_pmap
  #pixel_df$ID <- cumsum(!duplicated(pixel_df[ ,id])) # OLD code
  pixel_df$ID <- cumsum(!duplicated(pixel_df[ ,"id"]))

  #define specific class to control what maps are used with build_pmap
  oldClass(pixel_df) <- c("pixel_df", class(pixel_df))



  pixel_df

}
