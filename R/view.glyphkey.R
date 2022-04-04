#'@export
#'@import "ggplot2"

view.glyphkey <- function(obj) {
  if (class(obj)[1] != "glyphkey")
    stop("Object is not of class 'glyphkey'.")

  if (isTRUE(obj$transparent)) {
    p <- ggplot() +
      geom_polygon(
        data = obj$main3,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .7
      ) +
      geom_polygon(
        data = obj$extra5,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .2
      ) +
      coord_equal() +
      geom_segment(aes(
        x = -5,
        xend = 10,
        y = -0,
        yend = 0
      ),
      colour = "black",
      alpha = .5) +
      geom_segment(aes(
        x = 0,
        xend = 0,
        y = 10.5,
        yend = -10.5
      ),
      colour = "black",
      alpha = .5) +
      geom_text(aes(
        x = 0,
        y = 12,
        label = obj$key_label
      ), size = 4) +
      geom_text(aes(
        x = -4,
        y = 7,
        label = obj$main3_labels[1]
      ), size = 4) +
      geom_text(aes(
        x = 11.1,
        y = 1.4,
        label = obj$main3_labels[2]
      ), size = 4) +
      geom_text(aes(
        x = -5.5,
        y = -7,
        label = obj$main3_labels[3]
      ), size = 4) +
      xlim(c(-9, 13)) +
      ylim(c(-11, 12)) +
      theme(
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  } else {
    p <- ggplot() +
      geom_polygon(
        data = obj$main3,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .7
      ) +
      geom_polygon(
        data = obj$extra5,
        aes_string(x = 'V1', y = 'V2', group = 'id'),
        fill = "black",
        alpha = .2
      ) +
      coord_equal() +
      geom_segment(aes(
        x = -5,
        xend = 10,
        y = -0,
        yend = 0
      ),
      colour = "black",
      alpha = .5) +
      geom_segment(aes(
        x = 0,
        xend = 0,
        y = 10.5,
        yend = -10.5
      ),
      colour = "black",
      alpha = .5) +
      geom_text(aes(
        x = 0,
        y = 12,
        label = obj$key_label
      ), size = 4) +
      geom_text(aes(
        x = -4,
        y = 7,
        label = obj$main3_labels[1]
      ), size = 4) +
      geom_text(aes(
        x = 11.1,
        y = 1.4,
        label = obj$main3_labels[2]
      ), size = 4) +
      geom_text(aes(
        x = -5.5,
        y = -7,
        label = obj$main3_labels[3]
      ), size = 4) +
      xlim(c(-9, 13)) +
      ylim(c(-11, 12)) +
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
