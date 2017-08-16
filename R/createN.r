#' Internal function to create random variables
#' Creates a Normal random variable
#'
#'@param x x object
#'@param est Estimate
#'@param error Error

#'@importFrom "stats" "rnorm"

createN <- function(x, est, error){


  mean <- unique(est[x])
  sd <- unique(error[x])
  values <- rnorm(length(x), mean = mean, sd = sd)
  values

}

