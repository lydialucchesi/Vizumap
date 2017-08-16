## ----echo = FALSE, fig.align='center', fig.height=6, fig.width=6, fig.asp=.5, warning=FALSE----
library(knitr)
library(ggplot2)

b <- data.frame(name = rev(c("read.uv", "build_bmap", "build_bkey", "build_palette", "view", "attach_key")),
                x = rep(0, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkgreen", "darkred", "darkred")))

p <- data.frame(name = rev(c("read.uv", "pixelate", "build_pmap", "animate", "view", "")), 
                x = rep(1, 6), 
                y = seq(0, 5, 1), 
                col = rev(c("midnightblue", "midnightblue", "darkgreen", "darkgreen", "darkred", "darkred")))


g <- data.frame(name = rev(c("read.uv", "build_gmap", "build_gkey", "view", "attach_key", "")), 
                x = rep(2, 6), 
                y = seq(0, 5, 1),
                col = rev(c("midnightblue", "darkgreen", "darkgreen", "darkred", "darkred", "darkred")))

functions <- rbind(b, p, g)

headings <- data.frame(name = c("Bivariate map", "Pixel map", "Glyph map"), x = c(0, 1, 2), y = rep(6, 3))

g <- ggplot() + geom_text(data = functions, aes(x = x, y = y, label = name, colour=col), size = 3, fontface = "bold") + 
  scale_colour_identity() + 
  geom_text(data = headings, aes(x = x, y = y, label = name), size = 4, fontface = "bold") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()) + lims(x = c(-1, 3), y = c(-.5, 7))
plot(g)

## ---- echo=TRUE, fig.align = "center", fig.width=6, fig.height=6, message = FALSE----
library(VizU)
library(rgdal)

data(us_data)
head(us_data)

data(us_geo)
plot(us_geo)

## ---- echo = TRUE, fig.align = "center", fig.width=2, fig.height=2-------
#use one of four prepared colour palettes
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

#format data frame
poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")

#check that estimates and errors are in the first two columns
head(poverty)

## ---- echo = TRUE, fig.align = "center", fig.width=8, fig.height=6, fig.asp=.75----
#build a bivariate map with a shapefile
m <- build_bmap(data = poverty, shapefile = us_geo, id = "GEO_ID", border = "state", terciles = TRUE)
view(m)

