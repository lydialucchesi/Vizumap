#'Format data
#'
#'This function formats a data frame so it can be used with
#'\code{\link{build_bmap}}, \code{\link{build_gmap}}, \code{\link{build_pmap}},
#'\code{\link{build_bkey}} and \code{\link{build_gkey}}.
#'
#'Estimates and errors must be in the first and second columns of a data frame
#'for \code{\link{build_bmap}}, \code{\link{build_gmap}},
#'\code{\link{build_pmap}}, \code{\link{build_bkey}} and
#'\code{\link{build_gkey}}. \code{\link{read.uv}} provides an automated way to
#'format a data frame already loaded in R or to import a CSV file as a data
#'frame and arrange the columns correctly.
#'
#'@param data A data frame.
#'@param file A CSV file pathway.
#'@param estimate Name of estimate column.
#'@param error Name of error column.
#'@param exceedance Name of exceedance probability column.
#'
#'@examples
#'data(us_data)
#'poverty <- read.uv(data = us_data, estimate = "pov_rate", error = "pov_moe")
#'head(poverty)
#'
#'@export
#'@importFrom "utils" "read.csv"


read.uv <- function(data = NULL, file = NULL, estimate, error, exceedance){


  if(is.null(data) & is.null(file))
    stop("One of data or file needs to be supplied to this function.\n")


  if(!is.null(file)){
    data <- read.csv(file)
  }

  if(missing(exceedance)){
    if(missing(estimate) | missing(error))
      stop("'estimate' and 'error' are the column names of the data frame representing the estimate and error.
           \n")

    m1 <- match(estimate, names(data))
    m2 <- match(error, names(data))

    data[ ,estimate] <- as.numeric(data[ ,estimate])
    data[ ,error] <- as.numeric(data[ ,error])

    nms <- names(data)
    m <- match(c(estimate, error), names(data))
    morder <- c(nms[m], nms[-m])
    dataR <- data[,morder]

  }
  else{
    m <- match(exceedance, names(data))
    if(is.na(m))
      stop("The exceedance probability needs to be one of the column names of the data.\n")

    data[, exceedance] <- as.numeric(data[,exceedance])
    nms <- names(data)
    morder <- c(nms[m], nms[-m])
    dataR <- data[,morder]
  }


  dataR


}
