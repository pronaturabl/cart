## shift and scale coordinates according to Cartogram@gridOrthogocanal
cartogram.shiftScaleCoords <- function(coords, Cartogram) {
  ## check arguments
if (!class(Cartogram) == "Cartogram") {
    stop("argument 'Cartogram' must be an object of class 'Cartogram'")      
      } 
      
    coords <- matrix(c(

     Cartogram@shift[1] / Cartogram@gridOrthogonal@grid@cellsize[1] + (coords[, 1] - Cartogram@bbox[1, 1]) / diff(Cartogram@bbox[1, ]) * diff(Cartogram@bbox[1, ]) / Cartogram@gridOrthogonal@grid@cellsize[1]

     ,

     Cartogram@shift[2] / Cartogram@gridOrthogonal@grid@cellsize[2] + (coords[, 2] - Cartogram@bbox[2, 1]) / diff(Cartogram@bbox[2, ]) * diff(Cartogram@bbox[2, ]) / Cartogram@gridOrthogonal@grid@cellsize[2]

     ), byrow = FALSE, ncol = 2)
  return(coords)
}