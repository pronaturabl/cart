## gridCart constructs the cartogram

cartogram.gridCartogram <- function(Cartogram) {
  ## check arguments
if (!class(Cartogram) == "Cartogram") {
    stop("argument 'Cartogram' must be an object of class 'Cartogram'")      
      } 

  ## spdf
  ## - must have class SpatialPolygonsDataFrame
  if (!class(Cartogram@spdf) == "SpatialPolygonsDataFrame")
    stop("Cartogram slot @spdf must contain an object of class 'SpatialPolygonsDataFrame'")

  ## grid
  ## - must have class SpatialGrid
  if (!class(Cartogram@gridOrthogonal) == "SpatialGrid")
    stop("Cartogram slot @grid must contain an object of class 'SpatialGrid'")
    
  ## variable
  ## - must have length 1
  if (length(Cartogram@variable) != 1)
    stop("Cartogram slot @variable must have length 1")
  ## - must be of type numeric or character
  if (!(is.numeric(Cartogram@variable) | is.character(Cartogram@variable)))
    stop("Cartogram slot @variable must be of type 'numeric' or 'character'")
  ## - must refer to a column in spdf
  check <- try(!Cartogram@spdf@data[, Cartogram@variable], silent = TRUE)
  if (class(check) == "try-error")
    stop("Cartogram@variable is not a valid data.frame column of Cartogram@spdf")

  ## nrows, ncols
  ## - must be coercable to type integer
  nrows <- as.integer(Cartogram@nrows)
  ncols <- as.integer(Cartogram@ncols)
  if (!is.integer(nrows))
    stop("Cartogram@nrows must be of type 'numeric' and coercable to type 'integer'")
  if (!is.integer(ncols))
    stop("Cartogram@ncols must be of type 'numeric' and coercable to type 'integer'")
  ## - must be > 0
  if (!nrows > 0)
    stop("Cartogram@nrows must be greater than 0")
  if (!ncols > 0)
    stop("Cartogram@ncols must be greater than 0")
  ## - should be power of 2, otherwise FFTW is slow
  if (!isTRUE(all.equal(log(nrows, 2), floor(log(nrows, 2)))))
    warning("Cartogram@nrows should be a power of 2 for faster calculation")
  if (!isTRUE(all.equal(log(ncols, 2), floor(log(ncols, 2)))))
    warning("Cartogram@ncols should be a power of 2 for faster calculation")
  ## create a grid

  ## The algorithm by Newman works best if there is a generous "sea" around
  ## the "land", thus By default, add 50% of the x/y ranges to each side and
  ## define a grid of 512x512 points. Because the C code of Mark Newman uses
  ## FFTW, the number of grid points should be a power of two in order for FFTW
  ## to work faster.
  bb <- bbox(Cartogram@spdf)
  range <- diff(t(bb))
  shift <- Cartogram@seaSize * range
  dim <- c(x = Cartogram@ncols, y = Cartogram@nrows)
  grid <- Cartogram@gridOrthogonal

  ## overlay grid and polygons

  ## This is an extension of the point-in-polygon problem. We obtain a vector of
  ## indices of the polygons in spdf.
  ind <- overlay(grid, Cartogram@spdf)


  ## calculate "density"

  ## For each grid cell, we need to determine the fraction of the units of
  ## "variable" as the number of units per cell. For NAs, i.e. for the "sea",
  ## insert the mean value for the whole "land" mass. For the tabulation, the
  ## levels need to be enforced because there might be polygons with count zero.
  ## For these cells, division by zero is corrected by replacing the resulting
  ## infinite result by zero.
  tab <- xtabs(~ factor(ind, levels = seq(along = Cartogram@spdf@polygons)))
  var <- Cartogram@spdf@data[, Cartogram@variable]
  indVar <- var[as.numeric(names(tab))] / tab
  indVar[is.infinite(indVar)] <- 0
  mean <- sum(tab * indVar[as.numeric(names(tab))]) / sum(tab)
  ind[is.na(ind)] <- length(var) + 1
  indVar[length(var) + 1] <- mean
  dens <- matrix(indVar[ind], byrow = TRUE, ncol = dim["x"])

  ## calculate the cartogram coordinates

  ## create two temporary files
  tmpDens <- tempfile("cart")
  tmpCoord <- tempfile("cart")

  ## write the density matrix to the temporary file
  ## (use the format expected by the standalone cart application, which
  ## includes that the rows are reverted)
  write.table(dens[rev(seq(along = dens[, 1])), ],
              file = tmpDens, sep = " ",
              row.names = FALSE, col.names = FALSE)

  ## call the cart application
  invisible(.C("main",
               as.integer(5),
               c("cart",
                 dim["x"],
                 dim["y"],
                 tmpDens,
                 tmpCoord),
               PACKAGE = "cart"))

  ## remove the first temporary file
  file.remove(tmpDens)

  coordsGrid <- read.table(tmpCoord, sep = " ")

  ## clean up
  file.remove(tmpCoord)

  return(coordsGrid)
}