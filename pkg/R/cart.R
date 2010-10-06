

##' Creates a cartogram.
##'
##' This function creates a cartogram from a
##' \code{\link[sp]{SpatialPolygonsDataFrame}} object using
##' the indicated data.frame variable.
##'
##' @author Thomas Zumbrunn \email{thomas@@zumbrunn.name}
##' @param spdf \code{\link[sp]{SpatialPolygonsDataFrame}} object for which
##' to create a cartogram
##' @param variable numeric or character indicating the data.frame column in
##' 'spdf' which to use for the density calculations
##' @return SpatialPolygonsDataFrame object
##' @references Gastner MT, Newman MEJ (2004) Diffusion-based method for
##' producing density equalizing maps. Proc. Natl. Acad. Sci. 101:7499-7504
##' @useDynLib cart
##' @export cartogram
##' @callGraph
##' @examples
##' data(usapop)
##' usmap <- map("state",
##'               fill = TRUE, plot = FALSE)
##' sp <- map2SpatialPolygons(usmap, sub(":.*", "", usmap$names))
##' rownames(usapop) <- tolower(rownames(usapop))
##' spdf <- SpatialPolygonsDataFrame(sp, usapop, match.ID = TRUE)
##' cart <- cartogram(spdf, "population")
##' plot(cart, axes = TRUE, asp = 1/2, col = "#147f14")

cartogram <- function(spdf,
                      variable = 1) {

  
  ## check arguments
  
  ## spdf
  ## - must have class SpatialPolygonsDataFrame
  if (!class(spdf) == "SpatialPolygonsDataFrame")
    stop("argument 'spdf' must be an object of class 'SpatialPolygonsDataFrame'")
  
  ## variable
  ## - must have length 1
  if (length(variable) != 1)
    stop("argument 'variable' must have length 1")
  ## - must be of type numeric or character
  if (!(is.numeric(variable) | is.character(variable)))
    stop("argument 'variable' must be of type 'numeric' or 'character'")
  ## - must refer to a column in spdf
  check <- try(!spdf@data[, variable], silent = TRUE)
  if (class(check) == "try-error")
    stop("argument 'variable' is not a valid data.frame column of argument 'spdf'")

  
  ## create a grid

  ## The algorithm by Newman works best if there is a generous "sea" around
  ## the "land", thus By default, add 50% of the x/y ranges to each side and
  ## define a grid of 1024x1024 points. Because the C code of Mark Newman uses
  ## FFTW, the number of grid points should be a powers of two in order for FFTW
  ## to work faster.
  bb <- bbox(spdf)
  range <- diff(t(bbox(spdf)))
  shift <- 0.5 * range
  dim <- c(2^10, 2^10)
  grid <- SpatialGrid(GridTopology(cellcentre.offset = as.numeric(bb[, "min"] - shift),
                                   cellsize = as.numeric(diff(t(bb + t(rbind(-shift, shift)))) / (dim - 1)),
                                   cells.dim = dim))


  ## overlay grid and polygons

  ## This is an extension of the point-in-polygon problem. We obtain a vector of
  ## indices of the polygons in spdf.
  ind <- overlay(grid, spdf)


  ## calculate "density"

  ## For each grid cell, we need to determine the fraction of the units of
  ## "variable" as the number of units per cell. For NAs, i.e. for the "sea",
  ## insert the mean value for the whole "land" mass.
  tab <- xtabs(~ ind)
  var <- spdf@data[, variable]
  indVar <- var[as.numeric(names(tab))] / tab
  mean <- sum(tab * indVar[as.numeric(names(tab))]) / sum(tab)
  ind[is.na(ind)] <- length(var) + 1
  indVar[length(var) + 1] <- mean
  dens <- matrix(indVar[ind], byrow = TRUE, ncol = dim[1])

  
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
  .C("main",
     as.integer(5),
     c("cart",
       dim[1],
       dim[2],
       tmpDens,
       tmpCoord),
     PACKAGE = "cart")

  ## remove the first temporary file
  file.remove(tmpDens)


  ## produce the cartogram by interpolation
  
  ## loop over all polygons
  PolygonsList <- list()
  coordsGrid <- read.table(tmpCoord, sep = " ")
  for (i in seq(along = spdf@polygons)) {
    PolygonList <- list()
    for (j in seq(along = spdf@polygons[[i]]@Polygons)) {
      Polygons <- spdf@polygons[[i]]@Polygons[[j]]
      coords <- Polygons@coords
      ## shift and scale coordinates according to grid
      coords <- matrix(c(shift[1] / grid@grid@cellsize[1] + (coords[, 1] - bb[1, 1]) / diff(bb[1, ]) * diff(bb[1, ]) / grid@grid@cellsize[1],
                         shift[2] / grid@grid@cellsize[2] + (coords[, 2] - bb[2, 1]) / diff(bb[2, ]) * diff(bb[2, ]) / grid@grid@cellsize[2]),
                       byrow = FALSE, ncol = 2)
      ## interpolate
      coordsTr <- interpolate(coords,
                              matrix(coordsGrid[, 1], byrow = FALSE, ncol = dim[1] + 1),
                              matrix(coordsGrid[, 2], byrow = FALSE, ncol = dim[2] + 1))
      PolygonList[[j]] <- Polygon(coordsTr, hole = FALSE)
    }
    ID <- spdf@polygons[[i]]@ID
    PolygonsList[[i]] <- Polygons(PolygonList, ID)
  }
  
  ## assemble object
  sp <- SpatialPolygons(PolygonsList, proj4string = spdf@proj4string)

  ## clean up
  file.remove(tmpCoord)
  

  ## return the newly created SpatialPolygons object
  return(sp)
  
}



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


interpolate <- function(xy, xgrid, ygrid) {

  ## adapted from Mark Newman's code in interp.c

  ## TODO: check input

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
  mat <- cbind((1 - dx) * (1 - dy) * xgrid[cbind(ix, iy)] + dx * (1 - dy) * xgrid[cbind(ix + 1, iy)] +
               (1 - dx) * dy * xgrid[cbind(ix, iy + 1)] + dx * dy * xgrid[cbind(ix + 1, iy + 1)],
               (1 - dx) * (1 - dy) * ygrid[cbind(ix, iy)] + dx * (1 - dy) * ygrid[cbind(ix + 1, iy)] +
               (1 - dx) * dy * ygrid[cbind(ix, iy + 1)] + dx * dy * ygrid[cbind(ix + 1, iy + 1)])

  ## if points are outside the grid, return them untransformed
  untransformed <- which(x < 0 | x >= xsize | y < 0 | y > ysize)
  if (length(untransformed) > 0)
    mat[untransformed, ] <- c(x, y)[untransformed, ]

  ## return the matrix
  return(mat)
  
}
