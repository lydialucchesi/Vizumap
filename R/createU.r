# internal functions to create random values
# uniform
createU <- function(x, est, error){

  up <- unique(est[x]) + unique(error[x])
  lo <- unique(est[x]) - unique(error[x])

  vec <- seq(lo, up, length.out=5)
  values <- sample(vec, length(x), replace=TRUE)
  values

}
