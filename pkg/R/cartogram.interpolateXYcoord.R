##' Performs bilinear interpolation.
##'
##' This function performs bilinear interpolation of pairs of x/y
##' coordinates based on a matrix of x gridcoordinates and a matrix of y
##' grid coordinates. The algorithm is based on C code by Mark Newman.
##'
##' @author Thomas Zumbrunn \email{thomas@@zumbrunn.name}
##' @param xy numeric matrix of x and y coordinate pairs to be transformed
##' @param xgrid numeric matrix of x grid coordinates
##' @param ygrid numeric matrix of y grid coordinates
##' @return matrix of x/y transformed coordinates
##' @nord


cartogram.interpolateXYcoord <- function(xy, Cartogram) {

  ## adapted from Mark Newman's code in interp.c

  ## TODO: check input
  ## TODO: document x/y and R's dim(nrow, ncol) coordinate system
  ##       and possible confusion that may arise from differences

if (!class(Cartogram) == "Cartogram") {
    stop("argument 'Cartogram' must be an object of class 'Cartogram'")
      }

 dim <- c(x = Cartogram@ncols, y = Cartogram@nrows)

## TODO: should we store this in the Cartogram for efficiency? (memoization)
  xgrid <- matrix(Cartogram@gridCartogram[, 1], byrow = TRUE, ncol = dim["x"] + 1)
  ygrid <- matrix(Cartogram@gridCartogram[, 2], byrow = TRUE, ncol = dim["x"] + 1)
  
  ## x/y vectors
  x <- xy[, 1]
  y <- xy[, 2]
  
  ## range
  xsize <- max(xgrid)
  ysize <- max(ygrid)

  ## indices
  ix <- round(x)
  iy <- round(y)

  ## rounding differences
  dx <- x - ix
  dy <- y - iy

  ## bilinear interpolation
  ## (note that matrix indexing works differently than in C)
  ## mat <- cbind((1 - dx) * (1 - dy) * xgrid[cbind(iy, ix)] + dx * (1 - dy) * xgrid[cbind(iy + 1, ix)] +
  ##              (1 - dx) * dy * xgrid[cbind(iy, ix + 1)] + dx * dy * xgrid[cbind(iy + 1, ix + 1)],
  ##              (1 - dx) * (1 - dy) * ygrid[cbind(iy, ix)] + dx * (1 - dy) * ygrid[cbind(iy + 1, ix)] +
  ##              (1 - dx) * dy * ygrid[cbind(iy, ix + 1)] + dx * dy * ygrid[cbind(iy + 1, ix + 1)])
  mat <- cbind((1 - dy) * (1 - dx) * xgrid[cbind(iy, ix)] + dy * (1 - dx) * xgrid[cbind(iy + 1, ix)] +
               (1 - dy) * dx * xgrid[cbind(iy, ix + 1)] + dy * dx * xgrid[cbind(iy + 1, ix + 1)],
               (1 - dy) * (1 - dx) * ygrid[cbind(iy, ix)] + dy * (1 - dx) * ygrid[cbind(iy + 1, ix)] +
               (1 - dy) * dx * ygrid[cbind(iy, ix + 1)] + dy * dx * ygrid[cbind(iy + 1, ix + 1)])

  ## if points are outside the grid, return them untransformed
  untransformed <- which(x < 0 | x >= xsize | y < 0 | y > ysize)
  if (length(untransformed) > 0)
    mat[untransformed, ] <- c(x, y)[untransformed, ]

  ## return the matrix
  return(mat)
  
}
