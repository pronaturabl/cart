  ## create a grid onto which the polygons are mapped

  ## The algorithm by Newman works best if there is a generous "sea" around
  ## the "land", thus By default, add 50% of the x/y ranges to each side and
  ## define a grid of 512x512 points. Because the C code of Mark Newman uses
  ## FFTW, the number of grid points should be a power of two in order for FFTW
  ## to work faster.

cartogram.gridOrthogonal <- function(Cartogram) {
if (!class(Cartogram) == "Cartogram") {
    stop("argument 'Cartogram' must be an object of class 'Cartogram'")      
      } 
  ## - should be power of 2, otherwise FFTW is slow
  if (!isTRUE(all.equal(log(Cartogram@nrows, 2), floor(log(Cartogram@nrows, 2)))))
    warning("Cartogram@nrows should be a power of 2 for faster calculation")
  if (!isTRUE(all.equal(log(Cartogram@ncols, 2), floor(log(Cartogram@ncols, 2)))))
    warning("Cartogram@ncols should be a power of 2 for faster calculation")
    bb <- Cartogram@bbox
  range <- diff(t(bb))
  shift <- Cartogram@seaSize * range
  dim <- c(x = Cartogram@ncols, y = Cartogram@nrows)
  grid <- SpatialGrid(GridTopology(cellcentre.offset = as.numeric(bb[, "min"] - shift),
                                   cellsize = as.numeric(diff(t(bb + t(rbind(-shift, shift)))) / (dim - 1)),
                                   cells.dim = dim))
  return(grid)
}
