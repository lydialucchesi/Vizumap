## ----echo = FALSE, fig.align='center', fig.height=4, fig.width=6, fig.asp=.6, warning=FALSE----
library(knitr)
library(ggplot2)

b <- data.frame(name = rev(c("read.uv", "build_bmap", "build_bkey", "build_palette", "view", "attach_key")),
                x = rep(0, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkgreen", "darkred", "darkred")))

p <- data.frame(name = rev(c("read.uv", "pixelate", "build_pmap", "animate", "view", "")), 
                x = rep(2, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "midnightblue", "darkgreen", "darkgreen", "darkred", "darkred")))


g <- data.frame(name = rev(c("read.uv", "build_gmap", "build_gkey", "view", "attach_key", "")), 
                x = rep(4, 6), 
                y = seq(0, 5, 1),
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkred", "darkred", "darkred")))

functions <- rbind(b, p, g)

headings <- data.frame(name = c("Bivariate map", "Pixel map", "Glyph map"), x = c(0, 2, 4), y = rep(6, 3))

g <- ggplot() + geom_text(data = functions, aes(x = x, y = y, label = name, colour=col), size = 4, fontface = "bold") + 
  scale_colour_identity() + 
  geom_text(data = headings, aes(x = x, y = y, label = name), size = 5, fontface = "bold") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()) + lims(x = c(-1, 5), y = c(-.5, 7))
plot(g)

## ---- echo=TRUE, fig.align = "center", fig.width=4, fig.height=4, message = FALSE----
library(VizU)

data(us_data)
head(us_data)

## ---- echo=FALSE, fig.align = "center", fig.width=4, fig.height=4, message = FALSE----
library(rgdal)

## ---- echo=TRUE, eval = TRUE, fig.align = "center", fig.width=4, fig.height=4, message = FALSE----
data(us_geo)
plot(us_geo)

## ---- echo = TRUE, fig.align = "center", fig.width=2, fig.height=2-------
#use one of four pre-prepared colour palettes
p <- build_palette(name = "CyanMagenta")
view(p)

## ---- echo = TRUE, fig.align = "center", fig.width=2, fig.height=2-------
#test two light colours with low values
#creates a bad palette
p <- build_palette(name = "usr", colrange = list(colour = c("tan2", "lightskyblue"), difC = c(1, 1)))
view(p)

#change colours for a better palette
p <- build_palette(name = "usr", colrange = list(colour = c("chartreuse4", "darkblue"), difC = c(1, 1)))
view(p)

#change difC values to increase colour differences
my_pal <- build_palette(name = "usr", colrange = list(colour = c("chartreuse4", "darkblue"), difC = c(3, 4)))
view(my_pal)

## ---- echo = TRUE, fig.align = "center"----------------------------------
#load data
data(us_data)
data(us_geo)

#current column order of us_data
head(us_data)

#format data frame
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

#estimates and errors are now in the first two columns
head(poverty)

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=6, fig.asp=.7----
#build a bivariate map with a shapefile
m <- build_bmap(data = poverty, shapefile = us_geo, id = "GEO_ID", border = "state", terciles = TRUE)
view(m)

## ---- echo = TRUE, fig.align = "center", fig.width=4, fig.height=4-------
#build a key
k <- build_bkey(data = poverty, terciles = TRUE)
view(k)

## ---- echo=TRUE, fig.align="center", fig.asp=.5, fig.height=10, fig.width=14, message=FALSE, warning=FALSE----
attach_key(m, k)

## ---- echo = TRUE, eval = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
#make some changes
m2 <- build_bmap(data = poverty, shapefile = us_geo, id = "GEO_ID", border = "world", terciles = FALSE, palette = my_pal)
view(m2)

## ---- echo = TRUE--------------------------------------------------------
data(us_data)
us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
ca_data <- subset(us_data, us_data$GEO.id2 > 6000 & us_data$GEO.id2 < 7000)
ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "pov_moe")
row.names(ca_data) <- seq(1, nrow(ca_data), 1)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  data(us_geo)
#  ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
#  pix <- pixelate(ca_geo, id = "region")

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  df <- data.frame(region = sapply(slot(ca_geo, "polygons"), function(x) slot(x, "ID")), name = unique(ca_geo@data$GEO_ID))
#  ca_data$region <- df[match(ca_data$GEO_ID, df$name), 1]
#  ca_data$region <- as.character(ca_data$region)
#  
#  #check that values in shared column match
#  ca_data$region %in% pix$region

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = FALSE---------------
#  #uniform distribution
#  u_m <- build_pmap(data = ca_data, distribution = "uniform", pixelGeo = pix, id = "region")
#  view(u_m)

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = FALSE---------------
#  #normal distribution
#  ca_data$se <- ca_data$pov_moe / 1.645
#  ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "se")
#  
#  n_m <- build_pmap(data = ca_data, distribution = "normal", pixelGeo = pix, id = "region")
#  view(n_m)

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = FALSE---------------
#  #experiment with discrete distribution
#  #exponential - example for q argument
#  ca_data.q <- with(ca_data, data.frame(p0.05 = qexp(0.05, 1/pov_rate), p0.25 = qexp(0.25, 1/pov_rate), p0.5 = qexp(0.5, 1/pov_rate), p0.75 = qexp(0.75, 1/pov_rate), p0.95 = qexp(0.95, 1/pov_rate)))
#  
#  head(ca_data.q)
#  
#  d_m <- build_pmap(data = ca_data, distribution = "discrete",
#                    pixelGeo = pix, id = "region", q = ca_data.q)
#  view(d_m)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  #animate the normal distribution map
#  a <- animate(n_m, aniLength = 30)
#  view(a)

## ---- echo = TRUE--------------------------------------------------------
data(us_data)
data(us_geo)

co_geo <- subset(us_geo, us_geo@data$STATE == "08")

us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
#build a glyph map
map <- build_gmap(data = co_data, shapefile = co_geo, id = "GEO_ID", size = 80, glyph = "icone", border = "state")
view(map)

## ---- echo = TRUE, fig.align = "center", fig.width=4, fig.height=4-------
#build a glyph key
key <- build_gkey(data = co_data, glyph = "icone")
view(key)

## ---- echo = TRUE, fig.align = "center", fig.width=10, fig.height=6, fig.asp=.5, message = FALSE----
attach_key(map, key)

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
#build a glyph map
map2 <- build_gmap(data = co_data, shapefile = co_geo, id = "GEO_ID", size = 70, border = "county", glyph = "semi", palette = "Reds")
view(map2)

