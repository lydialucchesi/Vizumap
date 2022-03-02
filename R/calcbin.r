#'@importFrom "stats" "quantile"


calcbin <- function(terciles, data, x, q, bin, width, min){

  estbin <- ifelse(terciles, quantile(data[ ,x], q), (bin * width + min))
  estbin
}
