#'@export
#'@importFrom "graphics" "plot"
#'@importFrom "animation" "ani.options" "saveHTML"

#function to view animation

view.anipixelmap <- function(obj) {

  if(class(obj)[1] != "anipixelmap")
    stop("Object is not of class 'anipixelmap'.")



  ani.options(interval = obj$flickerSpeed)

  newobj <- obj[1:(length(obj)-1)]

  #plot maps in succession for animation
  gen_anim <- function() {
    for(i in 1:length(newobj)){
      m <- view(newobj[[i]])
      plot(m)
    }
  }


  #set time between frames and save animation
  saveHTML(gen_anim(), autoplay = FALSE, htmlfile = "animation.html", verbose = FALSE)


}

