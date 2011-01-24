.cartogram.buildFromSpdf <- function(.Object, spdf) {
if (!class(.Object) == "Cartogram") {
    stop("argument '.Object' must be an object of class 'Cartogram'")      
      } 
if (!class(spdf) == "SpatialPolygonsDataFrame") {
    stop("argument 'spdf' must be an object of class 'SpatialPolygonsDataFrame'")      
      } 
      .Object@spdf <- spdf
      .Object@bbox <- bbox(spdf)
      .Object@range <- diff(t(.Object@bbox))
      .Object@shift <- .Object@range * .Object@seaSize
      .Object@gridOrthogonal <- cartogram.gridOrthogonal(.Object)
      .Object@gridCartogram <- cartogram.gridCartogram(.Object)
      #return:
      .Object
}