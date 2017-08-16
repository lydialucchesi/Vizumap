# internal functions to create random values
# Discrete
createD <- function(x, q){

  qsub <- q[x,]
  qsubUQ <- unique(qsub)

  values <- sample(as.numeric(qsubUQ), length(x), replace=TRUE)
  values

}
