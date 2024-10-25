#'@export
#'@import "ggplot2"

view.bivmap <- function(obj) {
  if (class(obj)[1] != "bivmap")
    stop("Object is not of class 'bivmap'.")

  if ("id" %in% names(obj$output_data)) {
    p <- ggplot() +
      geom_polygon(
        data = obj$output_data,
        aes_string(
          x = 'long',
          y = 'lat',
          group = 'group',
          fill = 'hex_code'
        ),
        colour = "black",
        size = .1
      ) +
      scale_fill_identity()  +
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
  } else if (is(obj$output_data, "sf")) {
    p <- ggplot() +
      geom_sf(
        data = obj$output_data,
        aes_string(fill = 'hex_code'),
        colour = "black",
        size = .1
      ) +
      scale_fill_identity() +
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
  else {
    if (length(obj) == 3) {
      p <- ggplot() +
        geom_point(data = obj$output_data,
                   aes_string(
                     x = 'long',
                     y = 'lat',
                     colour = 'hex_code'
                   )) +
        scale_color_identity() +
        geom_path(
          data = obj$bord,
          aes_string(x = 'long', y = 'lat', group = 'group'),
          colour = "black"
        ) +
        coord_fixed(
          xlim = c(obj$bbox[1], obj$bbox[3]),
          ylim = c(obj$bbox[2], obj$bbox[4])
        )
    } else
      p <- ggplot() +
        geom_point(
          data = obj$output_data,
          aes_string(x = 'long', y = 'lat', colour = 'hex_code'),
          size = obj$size
        ) +
        scale_color_identity() +
        geom_path(
          data = obj$bord,
          aes_string(x = 'long', y = 'lat', group = 'group'),
          colour = "black"
        ) +
        coord_fixed(xlim = c(obj$bbox[1], obj$bbox[3]),
                    ylim = c(obj$bbox[2], obj$bbox[4]))
  }
  p

}
