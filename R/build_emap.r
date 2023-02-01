#'Build an exceedance probability colour map
#'
#'This function builds a map that visualises the probability of exceeding some
#'nominated threshold of concern.
#'
#'If \code{geoData} remains \code{NULL}, the function will produce a map of
#'plotted points representing specific sites; in this case, the data frame must
#'include latitude and longitude coordinates in columns \code{"long"} and
#'\code{"lat"}.
#'
#'@param data A data frame containing columns that house estimates of the mean,
#'standard deviation (or margin of error) and exceedance probability (optional).
#'A number of options are considered for calculating the probability of exceeding
#'a threshold. See below for more information.
#'@param pdflist A list capturing the pdf function that defines the distribution function to use to
#'calculate the probabilities of exeedence. By default this is NULL and assumes
#'the exceedance probabilities have been calculated outside of the function and passed
#'as a third column of the data dataframe.  Functions need to conform to the class
#'of distribution functions available within R through the \code{stats} package.
#'@param geoData A spatial polygons data frame.
#'@param id Name of the common column shared by the objects passed to
#'  \code{data} and \code{geoData}. The exceedance probability in the data frame
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
#'  \code{"YlOrBr"}, \code{"YlOrRd"}, \code{"YlGnBu"} and \code{"PuBuGn"}.
#'@param size An integer between 1 and 20. Value controls the size of points
#'  when \code{geoData = NULL}. If \code{size = NULL}, the points will remain
#'  the default size.
#'
#'  @details An exceedance probability map can be produced using:
#'  (i) precalculated exceedance probabilities, which are provided as a third column
#'  to the input dataframe; or
#'  (ii) exceedance probabilities that are calculated within the function using one of
#'  the standard probability distributions (e.g. \code{pnorm}) provided in the \code{stats}
#'  package in R, or
#'  (iii) exceedance probabilities that are calculated through a user defined function that
#'  is passed to the package which conforms to a similar structure as the suite of
#'  \code{Distributions} available in R.  Examples are provided below.
#'
#'@examples
#'data(us_data)
#'data(us_geo)
#'poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")
#'
#'# Exceedance probability map: Pr[X > 30] (Exponential Distribution)
#'#---- define probability distribution
#'pd <- quote({ pexp(q, rate, lower.tail = FALSE) })
#'#---- define argument listing
#'args <- quote({ list(rate = 1/estimate) })
#'#---- capture distribution and arguments in a single list
#'pdflist <- list(dist = pd, args = args, th = 30)
#'map <- build_emap(data = poverty, pdflist = pdflist, geoData = us_geo, id = "GEO_ID",
#'             border = "state", key_label = "Pr[X > 30]")
#'view(map) + ggplot2::ggtitle("Proper use of build_emap (appropriate distribution choice)")
#'
#'# Example where an inappropriate distributions is tried
#'# Exceedance probability map: Pr[X>30] (Normal Distribution)
#'
#'#---- define probability distribution
#'pd <- quote({ pnorm(q, mean, sd, lower.tail = FALSE) })
#'#---- define argument listing
#'args <- quote({ list(mean = estimate, sd = error/1.645) })
#'#---- capture distribution and arguments in a single list
#'pdflist <- list(dist = pd, args = args, th = 30)
#'map <- build_emap(data = poverty, pdflist = pdflist, geoData = us_geo, id = "GEO_ID",
#'             border = "state", key_label = "Pr[X > 30]")
#'view(map) + ggplot2::ggtitle("Misuse of build_emap (inappropriate distribution choice)")
#'
#'# Example where exceedance probabilities have been supplied (GBR Example)
#' # Load Upper Burdekin Data
#' data(UB)
#'
#'# Build Palette
#' exc_pal <- build_palette(name = "usr", colrange = list(colour = c("yellow", "red"),
#'                                                        difC = c(1, 1)))
#'                                                        view(exc_pal)
#'# Create map and view it
#' tss <- read.uv(data = UB_tss, estimate = "TSS", error = "TSS Error", exceedance = "TSS_exc1")
#' map <- build_emap(data = tss,  geoData = UB_shp, id = "scID",
#'             key_label = "Pr[TSS > 837mg/L]")
#' view(map)
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



build_emap <- function(data, pdflist = NULL, geoData = NULL, id = NULL, key_label,
                       palette = "YlOrRd", size = NULL, border = NULL) {


  if (is.null(geoData)) {
    l1 <- match("long", names(data))
    l2 <- match("lat", names(data))
    if (any(is.na(c(l1, l2))))
      stop("There must be coordinates in data columns named long and lat.\n")
  }

  if (!is.null(geoData) & is.null(id)) {
    stop("Missing id. Must be a common column shared by data and geoData.")
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


  # determine whether geoData has been entered by user
  # if so, link geoData and data and plot
  if (!is.null(geoData)) {

    geoData@data <- geoData@data %>% dplyr::mutate_if(is.factor,
                                                          as.character)
    geoData@data <- left_join(geoData@data, data, by = id)
    geoData@data$id <- rownames(geoData@data)
    region_coord <- sptable(geoData, region = "id")
    region_coord <- plyr::rename(region_coord, c(object_ = "id",
                                                 x_ = "long", y_ = "lat", branch_ = "group"))
    output_data <- plyr::join(region_coord, geoData@data, by = "id")
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)

  }
  else {
    output_data <- data
    bbox <- make_bbox(lat = lat, lon = long, data = output_data)
  }

  if(is.null(pdflist)){
    # assume 3 columns (pr_exc, estimate, error)
    id <- match(names(data)[1:3], names(output_data))
    names(output_data)[id] <- c("pr_exc", "estimate", "error")
   }
  else{
    # assume 2 columns and pr_exc is to be calculated via a function
    warning("Ensure the pdf you select is suitable for your data. See ??build_emap for examples of good and bad distribution choices.\n")

    id <- match(names(data)[1:2], names(output_data))
    names(output_data)[id] <- c("estimate", "error")
    estimate <- output_data$estimate
    error <- output_data$error

    args_call <- eval(do.call("substitute", list(pdflist$args, list(estimate, error))))
    output_data$pr_exc <- pdist(pname = pdflist$dist, th = pdflist$th, args = args_call)

  }



  p <- list(output_data = output_data, bord = bord, bbox = bbox,
            key_label = key_label, palette = palette)

  oldClass(p) <- c("emap", class(p))

  p

}
