library(Vizumap)
library(ggplot2)
library(dplyr)
library(gridExtra)

# load Upper Burdekin catchment data
data(UB)

# make bivariate map figure
UB_pal <-
  build_palette(name = "usr", colrange = list(colour = c("gold", "red4"),
                                              difC = c(4, 4)))
view(UB_pal)

dat <-
  read.uv(data = UB_tss,
          estimate = "TSS",
          error = "TSS_error")

key <- build_bkey(data = dat,
                  palette = UB_pal,
                  terciles = TRUE)

map <-
  build_bmap(
    data = dat,
    geoData = UB_shp,
    id = "scID",
    palette = UB_pal,
    terciles = TRUE
  )

map <- view(map) + coord_fixed()
key <- view(key) + coord_fixed()

lay <- rbind(c(1, 1, 1, 1, 2, 2),
             c(1, 1, 1, 1, 2, 2),
             c(1, 1, 1, 1, 2, 2))

plot <- grid.arrange(map, key, layout_matrix = lay)



# make pixel map figure
# an rda object that we made that contains a data frame with rel freq distr quantiles (amc95)
# not included in Vizumap package
#
# load("UB_Joss.rda")
#
# pixUB <- pixelate(UB_shp, id = "region")
#
# df <-
#   data.frame(region = sapply(slot(UB_shp, "polygons"), function(x)
#     slot(x, "ID")),
#     name = unique(UB_shp@data$scID))
#
# amc95$region <- df[match(amc95$scID, df$name), 1]
# amc95$region <- as.character(amc95$region)
#
# amc95$region %in% pixUB$region
#
# amc95_q <- amc95[, 3:15]
#
# dat <-
#   read.uv(data = amc95,
#           estimate = "TSS",
#           error = "TSS_error")
#
# map <-
#   build_pmap(
#     data = dat,
#     distribution = "discrete",
#     pixelGeo = pixUB,
#     id = "region",
#     palette = "Oranges",
#     q = amc95_q,
#     border = UB_shp
#   )
#
# map <- view(map) + coord_fixed()



# make glyph map figure
dat <- read.uv(data = UB_tss,
               estimate = "TSS",
               error = "TSS_error")

map <-
  build_gmap(
    data = dat,
    geoData = UB_shp,
    id = "scID",
    size = 1,
    glyph = "icone",
    palette = "Oranges",
    border = NULL
  )

key <- build_gkey(data = dat, glyph = "icone")

map <- view(map) + coord_fixed()
key <- view(key) + coord_fixed()

lay <- rbind(c(1, 1, 1, 1, 2, 2),
             c(1, 1, 1, 1, 2, 2),
             c(1, 1, 1, 1, 2, 2))

plot <- grid.arrange(map, key, layout_matrix = lay)



# make exceedance map figure
dat <-
  read.uv(data = UB_tss,
          estimate = "TSS",
          error = "TSS_error")

map <-
  build_emap(
    data = dat,
    geoData = UB_shp,
    id = "scID",
    key_label = "Pr[TSS > 837mg/L]"
  )

map <- view(map) + coord_fixed()
plot(map)
