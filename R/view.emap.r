#'@import "ggplot2"
#'@export

view.emap <- function(obj){

  if(class(obj)[1] != "emap")
    stop("Object is not of class 'emap'.")


  p <- ggplot() +
    geom_polygon(data = obj$output_data,
                 aes_string(x = 'long', y = 'lat', group = 'group',
                            fill = "Exceedance"),
                 colour = "black", size = .1) +
    scale_fill_distiller(direction=1, palette = obj$palette, name = obj$key_label, limits = c(0,1)) +
    scale_colour_distiller(direction=1, palette = obj$palette, limits = c(0,1)) +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank())
  p
}
