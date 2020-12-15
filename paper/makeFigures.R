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



# # make pixel map figure
# # an rda object that we made that contains a data frame with rel freq distr quantiles (amc95)
# # not included in Vizumap package
#
# load("UB_Joss.rda")
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
# all(amc95$region %in% pixUB$region)
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
# subdat <-
#   as.data.frame(subset(
#     map$output_data,
#     map$output_data$region %in% c("453", "474", "508", "491", "466")
#   ))
# subbord <-
#   as.data.frame(subset(map$bord, map$bord$id %in% c("453", "474", "508", "491", "466")))
#
# zoom <- ggplot()  +
#   geom_polygon(data = subdat,
#                aes(
#                  x = long,
#                  y = lat,
#                  fill = values,
#                  colour = values,
#                  group = group
#                ))  +
#   scale_fill_distiller(
#     direction = 1,
#     palette = map$palette,
#     limits = c(min(map$output_data$values), max(map$output_data$values))
#   ) +
#   scale_colour_distiller(
#     direction = 1,
#     palette = map$palette,
#     limits = c(min(map$output_data$values), max(map$output_data$values))
#   ) +
#   guides(colour = FALSE, fill = FALSE)  +
#   geom_path(data = subbord,
#             aes(x = long, y = lat, group = group),
#             colour = "black")  +
#   theme(
#     axis.line = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.background = element_blank(),
#     plot.caption = element_text(size = 15, hjust = 0)
#   ) +
#   labs(caption = "(B)")
#
# mapBord <-
#   view(map) + geom_path(
#     data = subbord,
#     aes(x = long, y = lat, group = group),
#     colour = "black",
#     size = 1
#   ) +
#   geom_segment(
#     aes(
#       x = 146.56,
#       y = -19.95,
#       xend = 146.837,
#       yend = -19.95
#     ),
#     arrow = arrow(length = unit(.4, "cm")),
#     size = .5
#   ) +
#   labs(caption = "(A)") +
#   theme(plot.caption = element_text(size = 15, hjust = 0))
#
# lay <- rbind(c(1, 1, 1, 1, NA, NA),
#              c(1, 1, 1, 1, 2, 2),
#              c(1, 1, 1, 1, 2, 2))
#
# plot <- grid.arrange(mapBord, zoom, layout_matrix = lay)



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
