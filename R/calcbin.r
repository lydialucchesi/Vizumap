#'@importFrom "stats" "quantile"


calcbin <- function(terciles, data, x, q, bin, width, min){

  estbin <- ifelse(terciles, round(quantile(data[ ,x], q), 2),
                          round(bin * width + min, 2))
  estbin
}
