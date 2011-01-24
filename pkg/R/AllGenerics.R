#' Generics for Cartogram class
#'

#' initialize a cartogram; if passed a \link{SpatialPolygonsDataFram},
#' will compute the cartogram distortion matrix
#' and store the transfomed \link{SpatialPolygons} in the @@cartogram slot
setMethod("initialize","Cartogram",
  function(.Object,spdf,...) {
    if(! missing(spdf)) {
      #do it
if (!class(spdf) == "SpatialPolygonsDataFrame") {
    stop("argument 'spdf' must be an object of class 'SpatialPolygonsDataFrame'")      
      } 
      .Object <- .cartogram.buildFromSpdf(.Object,spdf)
      #should we stop with that?
      .Object@cartogram <- cartogram.interpolatePolygons(spdf@polygons, .Object)
      } else {
       callNextMethod(.Object,...)
    }
      .Object
  } #function
  ) #setMethod initialize

#' The default \link{plot} of a Cartogram
#' plots the \link{SpatialPolygons} stored in the @@cartogram slot
setMethod("plot",signature(x="Cartogram",y="missing"),
  function(x,y,...) {
    plot(x@cartogram, ...)
    } #function
  ) #setMethod plot

#' \link{print}ing a cartogram is handled by \link{print.cartogram}
setMethod("print","Cartogram",
  function(x,...) {
    print.cartogram(x, ...)
    } #function
  ) #setMethod print

#' \link{show}ing a cartogram is handled by \link{print.cartogram}
setMethod("show","Cartogram",
  function(object) {
    print.cartogram(object)
    } #function
  ) #setMethod show
