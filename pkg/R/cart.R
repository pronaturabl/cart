

### Copyright (C) 2001 Thomas Zumbrunn <thomas@zumbrunn.name>
###
### This file is part of the cart package for R.
### It is made available under the terms of the GNU General Public
### License, version 3, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA



cartogram <- function(spdf,
                      variable = 1) {

  
  ## check arguments
  
  ## spdf
  ## - must have class SpatialPolygonsDataFrame
  if (!class(spdf) == "SpatialPolygonsDataFrame")
    stop("argument 'spdf' must be an object of class 'SpatialPolygonsDataFrame'")
  
  ## variable
  ## - must have length 1
  if (length(variable) != 1) {
    stop("argument 'variable' must have length 1")
  ## - must be of type numeric or character
  } else if (!(is.numeric(variable) | is.character(variable))) {
    stop("argument 'variable' must be of type 'numeric' or 'character'")
  }
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
  tmpDens <- "tmpDens.tmp"#tempfile("cart")
  tmpCoord <- "tmpCoord.tmp"#tempfile("cart")

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
       tmpCoord))

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
      coordsTr <- interpolate(coords[, 1], coords[, 2],
                              matrix(coordsGrid[, 1], byrow = FALSE, ncol = dim[1] + 1),
                              matrix(coordsGrid[, 2], byrow = FALSE, ncol = dim[2] + 1),
                              dim[1], dim[2])
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



interpolate <- function(x, y, xgrid, ygrid, xsize, ysize) {

  ## adapted from Mark Newman's code in interp.c

  ## TODO: check input
  ## TODO: vectorise

  ## loop over input
  mat <- apply(cbind(x, y), 1, function(arg) {
    x <- arg[1]
    y <- arg[2]
    ## if points are outside the grid, return them untransformed
    if (x < 0 | x >= xsize | y < 0 | y > ysize) {
      return(c(x, y))
    ## else interpolate
    } else {
      ix <- round(x)
      dx <- x - ix
      iy <- round(y)
      dy <- y - iy;
      return(c((1 - dx) * (1 - dy) * xgrid[ix, iy] + dx * (1 - dy) * xgrid[ix + 1, iy] +
               (1 - dx) * dy * xgrid[ix, iy + 1] + dx * dy * xgrid[ix + 1, iy + 1],
               (1 - dx) * (1 - dy) * ygrid[ix, iy] + dx * (1 - dy) * ygrid[ix + 1, iy] +
               (1 - dx) * dy * ygrid[ix, iy + 1] + dx * dy * ygrid[ix + 1, iy + 1]))
    }
  })

  ## return the matrix
  return(t(mat))
  
}
