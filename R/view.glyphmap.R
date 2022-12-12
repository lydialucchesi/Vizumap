#'@export
#'@import "ggplot2"

view.glyphmap <- function(obj){

  if(class(obj)[1] != "glyphmap")
    stop("Object is not of class 'glyphmap'.")

  p <- ggplot() +
    geom_polygon(data = obj$output_data, aes_string(x = 'long', y = 'lat', group = 'id', fill = 'val')) +
    scale_fill_distiller(direction=1, palette = obj$palette, name = obj$key_label, limits = obj$limits)  +
    geom_path(data = obj$bord, aes_string(x = 'long', y = 'lat', group = 'group'), colour = "black")  +
    coord_fixed(xlim = c(obj$bbox[1], obj$bbox[3]), ylim = c(obj$bbox[2], obj$bbox[4])) +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank())

  p

}

