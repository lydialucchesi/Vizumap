#'@importFrom "reshape2" "melt"

createPixrv <- function(pixelGeo, distribution){

  if(distribution == "uniform")

     rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, est, error) createU(x, est, error),  est = pixelGeo$estimate, error = pixelGeo$error)
  else
    if(distribution == "normal")
      rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, est, error) createN(x, est, error), est = pixelGeo$estimate, error = pixelGeo$error)
    else
      if(distribution == "discrete")
        rvarray <- tapply(1:nrow(pixelGeo), pixelGeo$ID, function(x, q) createD(x, q), q = pixelGeo$q)

  rvmelt <- melt(rvarray)
  values <- unlist(rvmelt$value)
  output_data <- pixelGeo
  output_data$values <- values
  output_data
}

