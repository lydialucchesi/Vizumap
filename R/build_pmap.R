#'Build a pixel map
#'
#'This function builds a choropleth map that visualises estimates and errors
#'simultaneously through pixelation and sampling.
#'
#'@param data A data frame.
#'@param distribution Name of the distribution that the pixel assignments will
#'  be drawn from. It must be one of \code{discrete}, \code{normal} or
#'  \code{uniform}. If \code{distribution = "discrete"}, a data frame of the
#'  quantiles that define the relative frequency distribution for the
#'  estimate must be entered for \code{q}. If \code{distribution = "normal"},
#'  the values assigned to pixels will be drawn from a normal distribution
#'  parameterised using the estimates and errors (means and standard
#'  deviations). If \code{distribution = "uniform"}, values will be sampled with
#'  equal probability from a sequence of 5 numbers that spans the estimate minus
#'  its error to the estimate plus its error.
#'@param pixelGeo An object from \code{\link{pixelate}}.
#'@param id Name of the common column shared by the objects passed to
#'  \code{data}, \code{pixelGeo} and \code{q} (if \code{distribution =
#'  "discrete"}).
#'@param border Name of geographical borders to be added to the map. It must be
#'  one of \code{\link[maps]{county}}, \code{\link[maps]{france}},
#'  \code{\link[maps]{italy}}, \code{\link[maps]{nz}},
#'  \code{\link[maps]{state}}, \code{\link[maps]{usa}} or
#'  \code{\link[maps]{world}} (see documentation for
#'  \code{\link[ggplot2]{map_data}} for more information). The borders will be
#'  refined to match latitute and longtidue coordinates provided in the data
#'  frame or spatial polygons data frame.
#'@param palette Name of colour palette. It must be one of \code{Blues},
#'  \code{Greens}, \code{Greys}, \code{Oranges}, \code{Purples} or \code{Reds}
#'  (see documentation for \code{\link[ggplot2]{scale_fill_distiller}} for more
#'  information).
#'@param q A data frame of quantiles which define the distribution for each
#'  estimate. Each row is an estimate, and each column is a quantile. See
#'  examples for an example of \code{q} input.
#'
#'
#'@seealso \code{\link{animate}}
#'
#'
#'@examples
#'# This code will produce a pixelated map when run in R
#'# It is not run here.
#'#data(us_geo)
#'#ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
#'#pix <- pixelate(ca_geo, id = "region")
#'
#'#data(us_data)
#'#us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
#'#ca_data <- subset(us_data, us_data$GEO.id2 > 6000 & us_data$GEO.id2 < 7000)
#'#ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "pov_moe")
#'#row.names(ca_data) <- seq(1, nrow(ca_data), 1)
#'
#'#df <- data.frame(region = sapply(slot(ca_geo, "polygons"),
#'#  function(x) slot(x, "ID")), name = unique(ca_geo@data$GEO_ID))
#'#ca_data$region <- df[match(ca_data$GEO_ID, df$name), 1]
#'#ca_data$region <- as.character(ca_data$region)
#'
#'#uniform distribution
#'#m <- build_pmap(data = ca_data, distribution = "uniform", pixelGeo = pix, id = "region")
#'#view(m)
#'
#'#normal distribution
#'#ca_data$se <- ca_data$pov_moe / 1.645
#'#ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "se")
#'
#'#m <- build_pmap(data = ca_data, distribution = "normal", pixelGeo = pix, id = "region")
#'#view(m)
#'
#'#experiment with discrete distribution
#'#exponential - example for q argument
#'#ca_data.q <- with(ca_data, data.frame(p0.05 = qexp(0.05, 1/pov_rate),
#'#  p0.25 = qexp(0.25, 1/pov_rate), p0.5 = qexp(0.5, 1/pov_rate),
#'#  p0.75 = qexp(0.75, 1/pov_rate), p0.95 = qexp(0.95, 1/pov_rate)))
#'
#'#m <- build_pmap(data = ca_data, distribution = "discrete", pixelGeo = pix,
#'#  id = "region", q = ca_data.q)
#'#view(m)
#'
#'@importFrom "ggmap" "make_bbox"
#'@export



build_pmap <- function(data = NULL, distribution = NULL, pixelGeo, id, border = NULL, palette = "Blues", q = NULL) {

  # option 1: sample from discrete probability distribution (distribution = "discrete")
  # option 2: sample from normal distribution (distribution = "normal")
  # option 3: sequence across confidence interval - uniform distribution (distribution = "uniform")


  #check that data or distribution is entered
  if (is.null(data) & is.null(distribution)) {
    stop("Missing data or distribution. Distribution needs to be one of 'uniform', 'normal' or 'discrete'.\n")
  }

  #check that map was pixelated with pixelate function
  if (class(pixelGeo)[1] != "pixel_df")
    stop("Object is not of class 'pixel_df.'\n")

  #check for id
  if (missing(id)) {
    stop("Missing id. Must be a common column shared by data and pixelGeo or distribution and pixelGeo.\n")
  }

  #check border name and load borders
  if (!is.null(border)) {
    if (border %in% c("county", "france", "italy", "nz", "state", "usa", "world"))
      bord <- map_data(border)
    else
      stop("Border name not recognised. Must be one of county, france, italy, nz, state, usa or world \n
           (see documentation for map_data function in ggplot2 for more information).\n")
  }
  else {
    long <- numeric(0)
    lat = numeric(0)
    bord <- data.frame(long = long, lat = lat, group = numeric(0))
  }

  #check palette name
  if (!(palette %in% c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds")))
    stop("Palette name not recognised. Must be one of Blues, Greens, Greys, Oranges, Purples or Reds \n
         (see documentation for scale_fill_distiller in ggplot2 for more information).\n")

  if(distribution == "discrete" & is.null(q))
    stop("A cumulative distribution function needs to be specified.\n")


  #match id classes and add estimate/error, mean/sd to pixel data frame
  if (!is.null(data)) {
    data[ ,id] <- as.character(data[ ,id])

    pixelGeo$estimate <- data[match(pixelGeo[ ,id], data[ ,id]), 1]
    pixelGeo$error <- data[match(pixelGeo[ ,id], data[ ,id]), 2]

    pixelGeo$estimate <- as.numeric(pixelGeo$estimate)
    pixelGeo$error <- as.numeric(pixelGeo$error)

    if(!is.null(q)){
      id <- match(pixelGeo[,id], data[,id])
      qdf <- q[id,]
      names(qdf) <- paste0("q", 1:ncol(q))

      pixelGeo$q <- qdf
    }

  }

  output_data <- createPixrv(pixelGeo, distribution)

  #define limits of border
  bbox <- make_bbox(lat = lat, lon = long, data = output_data)

  p <- list(output_data = output_data, bord = bord, bbox = bbox, key_label = names(data)[1], palette = palette, distribution = distribution)

  oldClass(p) <- c("pixelmap", class(p))

  p

}
