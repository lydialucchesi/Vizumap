#'@export

animate.pixelmap <- function(pmap, flickerSpeed = 0.05 , aniLength) {

  #check that map was pixelated with pixelate function
  if (class(pmap)[1] != "pixelmap")
    stop("Object is not of class 'pixelmap.'\n")

  #check for aniLength
  if (missing(aniLength)) {
    stop("Missing aniLength. Must be an integer greater than number of columns in data or distribution.\n")
  }

  #check for aniLength
  if (flickerSpeed > 1) {
    stop("The flicker speed must be a value less than or equal to .5.\n")
  }


  new_out_ani <- vector("list", length = aniLength)
  for(i in 1:aniLength){
    newvalues <- unlist(tapply(pmap$output_data$values, pmap$output_data$ID,
                               function(x)  sample(x, length(x), replace = TRUE)))
    new_out_ani[[i]] <- pmap
    new_out_ani[[i]]$output_data$values <- newvalues
  }



  p <- new_out_ani
  p$flickerSpeed <- flickerSpeed

  oldClass(p) <- c("anipixelmap", class(p))

  p

}
