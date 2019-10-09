#' @export
findNbounds <- function(data, estimate, error, terciles){


  #max and mins
  max_estimate <- max(data[ ,estimate])
  min_estimate <- min(data[ ,estimate])

  max_error <- max(data[ ,error])
  min_error <- min(data[ ,error])

  #find width of a color bin for equal interval option
  width_estimate <- (max_estimate  - min_estimate) / 3
  width_error <- (max_error  - min_error) / 3

  #find numerical bounds
  estimate_bin1 <- calcbin(terciles = terciles, data = data, x = estimate, q = 1/3, bin = 1, width = width_estimate,
                          min = min_estimate)
  estimate_bin2 <- calcbin(terciles = terciles, data = data, x = estimate, q = 2/3, bin = 2, width = width_estimate,
                           min = min_estimate)
  estimate_bin3 <- calcbin(terciles = terciles, data = data, x = estimate, q = 1, bin = 3, width = width_estimate,
                           min = min_estimate)

  error_bin1 <- calcbin(terciles = terciles, data = data, x = error, q = 1/3, bin = 1, width = width_error,
                        min = min_error)
  error_bin2 <- calcbin(terciles = terciles, data = data, x = error, q = 2/3, bin = 2, width = width_error,
                        min = min_error)
  error_bin3 <- calcbin(terciles = terciles, data = data, x = error, q = 1, bin = 3, width = width_error,
                        min = min_error)


  #creating a data frame of labels for the color key grid
  bound <- c(round(min_estimate, 2), estimate_bin1, estimate_bin2, estimate_bin3,
             round(min_error, 2), error_bin1, error_bin2, error_bin3)

  bound
}
