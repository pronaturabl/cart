##' Creates a cartogram.
##'
##' This function creates a cartogram from a
##' \code{\link[sp]{SpatialPolygonsDataFrame}} object using
##' the indicated data.frame variable.
##'
##  Warning: function deprecated and overridden by S4 generic method
##  of cartogram(x=SpatialPolygonsDataFrame,cart=missing)
##
##  This file/code retained (for now) for reference as the S4 methods still
##  need a bit of streamlining/refactoring
##'
##' @author Thomas Zumbrunn \email{thomas@@zumbrunn.name}
##' @param spdf \code{\link[sp]{SpatialPolygonsDataFrame}} object for which
##' to create a cartogram
##' @param variable numeric or character indicating the data.frame column in
##' 'spdf' which to use for the density calculations
##' @param nrows numeric, number of rows of the sampling grid
##' @param ncols numeric, number of columns of the sampling grid
##' @return SpatialPolygonsDataFrame object
##' @references Gastner MT, Newman MEJ (2004) Diffusion-based method for
##' producing density equalizing maps. Proc. Natl. Acad. Sci. 101:7499-7504
##' @useDynLib cart
##' @importFrom rdyncall dynbind .dyncall.default
##' @export cartogram
##' @examples
##' library(maps)
##' library(maptools)
##' library(sp)
##' data(usapop)
##' usapop <- usapop[-c(2, 12, 45), , drop = FALSE]
##' usmap <- map("state", fill = TRUE, plot = FALSE)
##' sp <- map2SpatialPolygons(usmap, sub(":.*", "", usmap$names))
##' rownames(usapop) <- tolower(rownames(usapop))
##' spdf <- SpatialPolygonsDataFrame(sp, usapop, match.ID = TRUE)
##' cart <- cartogram(spdf, "population")
##' plot(cart, axes = TRUE, asp = 1/2, col = "#147f14")
cartogram <- function(spdf,
                      variable = 1,
                      nrows = 2^8,
                      ncols = 2^8) {

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

  ## nrows, ncols
  ## - must be coercable to type integer
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  if (!is.integer(nrows))
    stop("argument 'nrows' must be of type 'numeric' and coercable to type 'integer'")
  if (!is.integer(ncols))
    stop("argument 'ncols' must be of type 'numeric' and coercable to type 'integer'")
  ## - must be > 0
  if (!nrows > 0)
    stop("argument 'nrows' must be greater than 0")
  if (!ncols > 0)
    stop("argument 'ncols' must be greater than 0")
  ## - should be power of 2, otherwise FFTW is slow
  if (!isTRUE(all.equal(log(nrows, 2), floor(log(nrows, 2)))))
    warning("argument 'nrows' should be a power of 2 for faster calculation")
  if (!isTRUE(all.equal(log(ncols, 2), floor(log(ncols, 2)))))
    warning("argument 'nrows' should be a power of 2 for faster calculation")


  ## create a grid

  ## The algorithm by Newman works best if there is a generous "sea" around
  ## the "land", thus by default, add 50% of the x/y ranges to each side and
  ## define a grid of 512x512 points. Because the C code of Mark Newman uses
  ## FFTW, the number of grid points should be a power of two in order for FFTW
  ## to work faster.
  bb <- bbox(spdf)
  range <- diff(t(bbox(spdf)))
  shift <- 0.5 * range  ## FIXME: If one wants to interpolate a coordinate that lies outside
                        ##        this region, function "interpolate" will fail!
  dim <- c(x = ncols, y = nrows)
  grid <- SpatialGrid(GridTopology(cellcentre.offset = as.numeric(bb[, "min"] - shift),
                                   cellsize = as.numeric(diff(t(bb + t(rbind(-shift, shift)))) / (dim - 1)),
                                   cells.dim = dim))


  ## overlay grid and polygons

  ## This is an extension of the point-in-polygon problem. We obtain a vector of
  ## indices of the polygons in spdf.
  ## FIXME: function 'overlay' is deprecated, use 'over' instead and adapt code.
  ind <- sp::overlay(grid, spdf)


  ## calculate "density"

  ## For each grid cell, we need to determine the fraction of the units of
  ## "variable" as the number of units per cell. For NAs, i.e. for the "sea",
  ## insert the mean value for the whole "land" mass. For the tabulation, the
  ## levels need to be enforced because there might be polygons with count zero.
  ## For these cells, division by zero is corrected by replacing the resulting
  ## infinite result by zero.
  tab <- xtabs(~ factor(ind, levels = seq(along = spdf@polygons)))
  var <- spdf@data[, variable]
  indVar <- var[as.numeric(names(tab))] / tab
  indVar[is.infinite(indVar)] <- 0
  mean <- sum(tab * indVar[as.numeric(names(tab))]) / sum(tab)
  ind[is.na(ind)] <- length(var) + 1
  indVar[length(var) + 1] <- mean
  dens <- matrix(indVar[ind], byrow = TRUE, ncol = dim["x"])

  ## calculate the cartogram coordinates

  ## call the modified standalone application
  gridx <- double((dim["x"] + 1) * (dim["y"] + 1))
  gridy <- double((dim["x"] + 1) * (dim["y"] + 1))
  rdyncall::dynbind(system.file(file.path("libs", paste("cart", .Platform$dynlib.ext, sep = "")), package = "cart"),
                    "embed_main(ii*d*d*d)v;")
  embed_main(dim["x"], dim["y"], as.double(t(dens[rev(seq(along = dens[, 1])), ])), gridx, gridy)
  coordsGrid <- cbind(gridx, gridy)

  ## produce the cartogram by interpolation

  ## loop over all polygons
  PolygonsList <- list()
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
                              matrix(coordsGrid[, 1], byrow = TRUE, ncol = dim["x"] + 1),
                              matrix(coordsGrid[, 2], byrow = TRUE, ncol = dim["x"] + 1))
      PolygonList[[j]] <- Polygon(coordsTr, hole = FALSE)
    }
    ID <- spdf@polygons[[i]]@ID
    PolygonsList[[i]] <- Polygons(PolygonList, ID)
  }

  ## assemble object
  sp <- SpatialPolygons(PolygonsList, proj4string = spdf@proj4string)

  ## return the newly created SpatialPolygons object
  return(sp)

}



##' Performs bilinear interpolation.
##'
##' This function performs bilinear interpolation of pairs of x/y
##' coordinates based on a matrix of x grid coordinates and a matrix of y
##' grid coordinates. The algorithm is based on C code by Mark Newman.
##'
##' @author Thomas Zumbrunn \email{thomas@@zumbrunn.name}
##' @param xy numeric matrix of x and y coordinate pairs to be transformed
##' @param xgrid numeric matrix of x grid coordinates
##' @param ygrid numeric matrix of y grid coordinates
##' @return matrix of x/y transformed coordinates
interpolate <- function(xy, xgrid, ygrid) {

  ## adapted from Mark Newman's code in interp.c

  ## TODO: check input
  ## TODO: document x/y and R's dim(nrow, ncol) coordinate system
  ##       and possible confusion that may arise from differences

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
  ## (note that matrix indexing works differently in R than in C)
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



##' Population data of the USA.
##'
##' @name usapop
##' @docType data
##' @format A data frame with 52 observations on the following variable.
##' \describe{
##' \item{\code{population}}{a numeric vector: estimated number of people living
##' in a specific U.S. state in 2009}
##' }
##' The state names are given in the data frame row names.
##' @source \link{http://en.wikipedia.org/wiki/List_of_U.S._States_by_population},
## retrieved 29 September 2010 19:04 UTC, Page Version ID: 383596724
##' @examples
##' data(usapop)
##' str(usapop)
##' @keywords data
NULL
