#'@export
#'@import "ggplot2"

view.pixelmap <- function(obj) {

  if(class(obj)[1] != "pixelmap")
    stop("Object is not of class 'pixelmap'.")

  if (inherits(obj$bord, "sf")) {
    p <- ggplot()  +
      geom_polygon(
        data = obj$output_data,
        aes_string(
          x = 'long',
          y = 'lat',
          fill = 'values',
          colour = 'values',
          group = 'group'
        )
      )  +
      scale_fill_distiller(
        direction = 1,
        palette = obj$palette,
        name = obj$key_label,
        limits = obj$limits
      ) +
      scale_colour_distiller(
        direction = 1,
        palette = obj$palette,
        limits = obj$limits
      ) +
      guides(colour = "none")  +
      geom_sf(
        data = obj$bord,
        colour = "black",
        fill = NA,
        linewidth = .5
      )  +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()
      )
  } else {
    p <- ggplot()  +
      geom_polygon(
        data = obj$output_data,
        aes_string(
          x = 'long',
          y = 'lat',
          fill = 'values',
          colour = 'values',
          group = 'group'
        )
      )  +
      scale_fill_distiller(
        direction = 1,
        palette = obj$palette,
        name = obj$key_label,
        limits = obj$limits
      ) +
      scale_colour_distiller(
        direction = 1,
        palette = obj$palette,
        limits = obj$limits
      ) +
      guides(colour = "none")  +
      geom_path(
        data = obj$bord,
        aes_string(x = 'long', y = 'lat', group = 'group'),
        colour = "black"
      )  +
      coord_fixed(xlim = c(obj$bbox[1], obj$bbox[3]),
                  ylim = c(obj$bbox[2], obj$bbox[4])) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank()
      )
  }

  p

}
