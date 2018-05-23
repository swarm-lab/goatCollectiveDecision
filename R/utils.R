#' @export
shift <- function(x, lag) {
  n <- length(x)
  xnew <- rep(NA, n)
  if (lag < 0) {
    xnew[1:(n - abs(lag))] <- x[(abs(lag) + 1):n]
  } else if (lag > 0) {
    xnew[(lag + 1):n] <- x[1:(n - lag)]
  } else {
    xnew <- x
  }
  xnew
}


#' @title Linear distances along a trajectory
#'
#' @description Given a set of cartesian coordinates representing an object's
#'  trajectory, this function computes the linear distances between each pair of
#'  successive locations along the trajectory.
#'
#' @param x A vector of x (or longitude) coordinates corresponding to a single
#'  animal trajectory.
#'
#' @param y A vector of y (or latitude) coordinates corresponding to a single
#'  animal trajectory.
#'
#' @param geo A logical value indicating whether the locations are defined by
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'
#' @return A vector of the same length as x and y corresponding to the linear
#'  distances between each pair of successive locations along the trajectory.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{linSpeed}}, \code{\link{linAcc}}, \code{\link{nsd}}
#'
#' @examples
#' # TODO
#'
#' @export
linDist <- function(x, y, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || length(x) != length(y)) {
    stop("x and y must be vectors of identical length.")
  }

  if (geo) {
    l <- length(x)
    m1 <- cbind(x[1:(l - 1)], y[1:(l - 1)])
    m2 <- cbind(x[2:l], y[2:l])
    c(0, geosphere::distGeo(m1, m2))
  } else {
    c(0, sqrt(diff(x) ^ 2 + diff(y) ^ 2))
  }
}


#' @export
bestLag <- function(x, y) {
  tmp <- ccf(x, y, plot = FALSE)
  tmp$lag[which.max(tmp$acf)]
}

#' @export
ccfSym <- function(x, y) {
  tmp <- ccf(x, y, plot = FALSE)
  idx <- which(tmp$lag == 0)
  sum(tmp$acf[1:(idx - 1)] - tmp$acf[length(tmp$acf):(idx + 1)])
}

