## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(ggplot2)
library(dplyr)

## ----echo = FALSE, fig.align='center', fig.height=5, fig.width=8, fig.asp=.6, warning=FALSE----

b <- data.frame(name = rev(c("read.uv", "build_bmap", "build_bkey", "build_palette", "view", "attach_key")),
                x = rep(0, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkgreen", "darkred", "darkred")))

p <- data.frame(name = rev(c("read.uv", "pixelate", "build_pmap", "animate", "view", "")), 
                x = rep(5, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "midnightblue", "darkgreen", "darkgreen", "darkred", "darkred")))


g <- data.frame(name = rev(c("read.uv", "build_gmap", "build_gkey", "view", "attach_key", "")), 
                x = rep(10, 6), 
                y = seq(0, 5, 1),
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkred", "darkred", "darkred")))

e <- data.frame(name = rev(c("read.uv", "build_emap", "view", "", "", "")), 
                x = rep(15, 6), 
                y = seq(0, 5, 1),
                col = rev(c("midnightblue", "darkgreen", "darkred", "darkred", "darkred", "darkred")))

functions <- rbind(b, p, g, e)

headings <- data.frame(name = c("Bivariate map", "Pixel map", "Glyph map", "Exceedance map"), x = c(0, 5, 10, 15), y = rep(6, 4))

g <- ggplot() + geom_text(data = functions, aes(x = x, y = y, label = name, colour=col), size = 4, fontface = "bold") + 
  scale_colour_identity() + 
  geom_text(data = headings, aes(x = x, y = y, label = name), size = 5, fontface = "bold") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()) + lims(x = c(-1, 17), y = c(-.5, 7))
plot(g)

## ---- echo=TRUE, fig.align = "center", fig.width=4, fig.height=4, message = FALSE----
library(Vizumap)

data(us_data)
head(us_data)

## ---- echo=TRUE, eval = TRUE, fig.align = "center", fig.width=4, fig.height=4, message = FALSE----
data(us_geo)

## ---- echo = TRUE, fig.align = "center", fig.width=2, fig.height=2------------
# use one of four pre-prepared colour palettes
cmBivPal <- build_palette(name = "CyanMagenta")
view(cmBivPal)

## ---- echo = TRUE, fig.align = "center", fig.width=2, fig.height=2------------
# test two light colours with low difC values
# creates a bad palette
customBivPal1 <- build_palette(name = "usr", colrange = list(colour = c("tan2", "lightskyblue"), difC = c(1, 1)))
view(customBivPal1)

# change colours for a better palette
customBivPal2 <- build_palette(name = "usr", colrange = list(colour = c("chartreuse4", "darkblue"), difC = c(1, 1)))
view(customBivPal2)

# change difC values to increase colour differences
customBivPal3 <- build_palette(name = "usr", colrange = list(colour = c("chartreuse4", "darkblue"), difC = c(3, 4)))
view(customBivPal3)

## ---- echo = TRUE, fig.align = "center"---------------------------------------
# load data
data(us_data)
data(us_geo)

# current column order of us_data
head(us_data)

# format data frame
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

# estimates and errors are now in the first two columns
head(poverty)

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=6, fig.asp=.7----
# build a bivariate map with the map data
usBivMap <- build_bmap(data = poverty, geoData = us_geo, id = "GEO_ID", terciles = TRUE)
view(usBivMap)

## ---- echo = TRUE, fig.align = "center", fig.width=4, fig.height=4------------
# build a key
usBivKey <- build_bkey(data = poverty, terciles = TRUE)
view(usBivKey)

## ---- echo=TRUE, fig.align="center", fig.asp=.65, fig.height=10, fig.width=14, message=FALSE, warning=FALSE----
attach_key(usBivMap, usBivKey)

## ---- echo = TRUE, eval = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
# make some changes
usBivMapDif <- build_bmap(data = poverty, geoData = us_geo, id = "GEO_ID", terciles = FALSE, palette = customBivPal3)
view(usBivMapDif)

## ---- echo = TRUE-------------------------------------------------------------
data(us_data)
us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
ca_data <- subset(us_data, us_data$GEO.id2 > 6000 & us_data$GEO.id2 < 7000)
ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "pov_moe")
row.names(ca_data) <- seq(1, nrow(ca_data), 1)

## ---- echo = TRUE, eval = TRUE------------------------------------------------
data(us_geo)
ca_geo <- subset(us_geo, us_geo@data$STATE == "06")
pix <- pixelate(ca_geo, id = "region")

## ---- echo = TRUE, eval = TRUE------------------------------------------------
df <- data.frame(region = sapply(slot(ca_geo, "polygons"), function(x) slot(x, "ID")), name = unique(ca_geo@data$GEO_ID))
ca_data$region <- df[match(ca_data$GEO_ID, df$name), 1]
ca_data$region <- as.character(ca_data$region)

# check that values in shared column match
all(ca_data$region %in% pix$region)

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = TRUE, message = FALSE----
# uniform distribution
unifPixMap <- build_pmap(data = ca_data, distribution = "uniform", pixelGeo = pix, id = "region", border = ca_geo)
view(unifPixMap)

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = TRUE, message = FALSE----
# normal distribution
ca_data$se <- ca_data$pov_moe / 1.645
ca_data <- read.uv(data = ca_data, estimate = "pov_rate", error = "se")

normPixMap <- build_pmap(data = ca_data, distribution = "normal", pixelGeo = pix, id = "region", border = ca_geo)
view(normPixMap)

## ---- echo = TRUE, fig.height=8, fig.width=8, eval = TRUE, message = FALSE----
# experiment with discrete distribution
# exponential - example for q argument
ca_data.q <- with(ca_data, data.frame(p0.05 = qexp(0.05, 1/pov_rate), p0.25 = qexp(0.25, 1/pov_rate), p0.5 = qexp(0.5, 1/pov_rate), p0.75 = qexp(0.75, 1/pov_rate), p0.95 = qexp(0.95, 1/pov_rate)))

head(ca_data.q)

discPixMap <- build_pmap(data = ca_data, distribution = "discrete", 
                  pixelGeo = pix, id = "region", q = ca_data.q, border = ca_geo)
view(discPixMap)

## ---- echo = TRUE, eval = TRUE------------------------------------------------
# animate the normal distribution map
normPixAni <- animate(normPixMap, aniLength = 30)
# view(normPixAni)

## ---- echo = TRUE-------------------------------------------------------------
data(us_data)
data(us_geo)

co_geo <- subset(us_geo, us_geo@data$STATE == "08")

us_data$GEO.id2 <- as.numeric(us_data$GEO.id2)
co_data <- subset(us_data, us_data$GEO.id2 > 8000 & us_data$GEO.id2 < 9000)
co_data <- read.uv(data = co_data, estimate = "pov_rate", error = "pov_moe")

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
# build a glyph map
usGlyphMap <- build_gmap(data = co_data, geoData = co_geo, id = "GEO_ID", size = 80, glyph = "icone", border = "state")
view(usGlyphMap)

## ---- echo = TRUE, fig.align = "center", fig.width=4, fig.height=4------------
# build a glyph key
usGlyphKey <- build_gkey(data = co_data, glyph = "icone")
view(usGlyphKey)

## ---- echo = TRUE, fig.align = "center", fig.width=10, fig.height=6, fig.asp=.5, message = FALSE----
attach_key(usGlyphMap, usGlyphKey)

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=8, fig.asp=.75----
# build a glyph map
usGlyphMapDif <- build_gmap(data = co_data, geoData = co_geo, id = "GEO_ID", size = 70, border = "county", glyph = "semi", palette = "Reds")
view(usGlyphMapDif)

## ---- echo = TRUE-------------------------------------------------------------
# load data
data(us_data)
data(us_geo)

# format the data
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

# check variable quantiles
quantile(us_data$pov_rate)

## ---- echo = TRUE-------------------------------------------------------------
# define probability distribution (exponential distribution)
pd <- quote({ pexp(q, rate, lower.tail = FALSE) })

# define argument listing
args <- quote({ list(rate = 1/estimate) })

# capture distribution and arguments in a single list
pdflist <- list(dist = pd, args = args, th = 30)

## ----echo=TRUE, fig.align="center", fig.asp=.6, fig.height=8, fig.width=8, message=FALSE, warning=FALSE----
usExcMap <- build_emap(data = poverty, pdflist = pdflist, geoData = us_geo, id = "GEO_ID", key_label = "Pr[X > 30]")
view(usExcMap)

