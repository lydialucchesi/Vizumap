#' pdist
#'
#' @description Calculates the probability of exceedance for a given distribution,
#' threshold and distribution parameters.
#'
#' @param pname Distribution to be used which is embedded through the \code{quote} command.
#' This can comprise an existing function from the \code{stats} package e.g. \code{pnorm}.
#' Alternatively this can be user defined.
#' @param th Threshold for calculating the \[Pr(X > th)\]
#' @param args Arguments that correspond to the specified distribution (but excluding the
#' threshold.
#'
#' @examples
#' pname <- quote({ pnorm(q, mean, sd, lower.tail = FALSE) })
#' pdist(pname = pname, th = 3, args = list(mean = 2, sd = 1))
#'
#'
#' @import stats
#' @export


pdist <- function(pname, th, args){

  args[["q"]] <- th
  dist_call <- do.call("substitute", list(pname, args))

  eval(dist_call)

}


