#'@importFrom "stats" "rnorm"

createN <- function(x, est, error){


  mean <- unique(est[x])
  sd <- unique(error[x])
  values <- rnorm(length(x), mean = mean, sd = sd)
  values

}

