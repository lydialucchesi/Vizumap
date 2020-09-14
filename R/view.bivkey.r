#'@export
#'@import "ggplot2"

view.bivkey <- function(obj){

  if(class(obj)[1] != "bivkey")
    stop("Object is not of class 'bivkey'")

  if(!obj$flipped) {
    x <- 'x'; y <- 'y'
  } else {
    x <- 'y'; y <- 'x'
  }

  # plot color key grid and add it to the user's environment so that it can be replotted if needed
  p <- ggplot() + geom_polygon(data = obj$tiles, aes_string(x = 'x', y = 'y', group = 'group', fill = 'color'),
                               colour = "black") +
    scale_fill_identity() +
    coord_equal() +
    theme(axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank()) +
    geom_text(aes(x = -.5, y = .1, label = paste0(obj$estimate), angle = -45), size = 4)  +
    geom_text(aes(x = 6.5, y = .1, label = paste0(obj$error), angle = 45), size = 4)  +
    geom_text(data = obj$labels, aes_string(x = x, y = y, label = 'bound', angle = 'angle'), size = 4) +
    xlim(c(-1.2, 7.2)) +
    ylim(c(-.75, 6))

  p

}
