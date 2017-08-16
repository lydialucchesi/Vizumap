#'@export
#'@import "ggplot2"

view.palette <- function(obj){


  if(class(obj)[1] != "palette")
    stop("Object is not of class 'palette'.")

  x1 <- c(3, 4, 3, 2) ; x2 <- c(4, 5, 4, 3) ; x3 <- c(5, 6, 5, 4)
  x <- c(x1, x1-1, x1-2, x2, x2-1, x2-2, x3, x3-1, x3-2)

  y1 <- c(0, 1, 2, 1) ; y2 <- c(1, 2, 3, 2) ; y3 <- c(2, 3, 4, 3)
  y <- c(y1, y1+1, y1+2, y2, y2+1, y2+2, y3, y3+1, y3+2)

  group <- rep(1:9, each = 4)

  clr <- rep(obj, each = 4)

  tiles <- data.frame(x = x, y = y, group = group, color = clr)



  #plot color key grid
  p <- ggplot() + geom_polygon(data = tiles, aes_string(x = 'x', y = 'y', group = 'group', fill = 'color'),
                               colour = "black") +
    scale_fill_identity() +
    coord_equal() +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank())

  p


}
