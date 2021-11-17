#' findNbounds
#'
#' Find the bins for the colour grid
#'
#' @param data Dataset from which bounds are to be determined.
#' @param estimate Name of the estimate column.
#' @param error Name of the error column.
#' @param terciles Should terciles be calculated? (Default: FALSE).
#' @param expertR_est A vector consisting of the range of expert values for the
#'   estimate (Default: NA).
#' @param expertR_err A vector consisting of the range of expert values for the
#'   error (Default: NA).
#'
#' @details Expert ranges for the estimate and error can be supplied to assist
#'   with the comparison of multiple figures. By default these are set to NA,
#'   which will result in the bounds being directly computed from the data.
#'   Note, if the expert values do not span the range of the data, this function
#'   will default to finding the bounds of the data.
#'
#' @export

findNbounds <-
  function (data,
            estimate,
            error,
            terciles,
            expertR_est = NA,
            expertR_err = NA)
  {
    max_estimate <- max(c(expertR_est, data[, estimate]), na.rm = TRUE)
    min_estimate <-
      min(c(expertR_est, data[, estimate]), na.rm = TRUE)
    max_error <- max(c(expertR_err, data[, error]), na.rm = TRUE)
    min_error <- min(c(expertR_err, data[, error]), na.rm = TRUE)
    width_estimate <- (max_estimate - min_estimate) / 3
    width_error <- (max_error - min_error) / 3

    estimate_bin1 <- calcbin(
      terciles = terciles,
      data = data,
      x = estimate,
      q = 1 / 3,
      bin = 1,
      width = width_estimate,
      min = min_estimate
    )
    estimate_bin2 <- calcbin(
      terciles = terciles,
      data = data,
      x = estimate,
      q = 2 / 3,
      bin = 2,
      width = width_estimate,
      min = min_estimate
    )
    estimate_bin3 <- calcbin(
      terciles = terciles,
      data = data,
      x = estimate,
      q = 1,
      bin = 3,
      width = width_estimate,
      min = min_estimate
    )
    error_bin1 <-
      calcbin(
        terciles = terciles,
        data = data,
        x = error,
        q = 1 / 3,
        bin = 1,
        width = width_error,
        min = min_error
      )
    error_bin2 <-
      calcbin(
        terciles = terciles,
        data = data,
        x = error,
        q = 2 / 3,
        bin = 2,
        width = width_error,
        min = min_error
      )
    error_bin3 <-
      calcbin(
        terciles = terciles,
        data = data,
        x = error,
        q = 1,
        bin = 3,
        width = width_error,
        min = min_error
      )
    bound <- c(
      round(min_estimate, 2),
      estimate_bin1,
      estimate_bin2,
      estimate_bin3,
      round(min_error, 2),
      error_bin1,
      error_bin2,
      error_bin3
    )
    bound
  }
