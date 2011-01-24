#' Declaration of Cartogram as a S4 class
#' 
#'
setClass("Cartogram", representation(
  spdf="SpatialPolygonsDataFrame",
  variable="ANY",
  bbox="matrix",
  nrows="numeric",
  ncols="numeric",
  range="matrix",
  shift="matrix",
  seaSize="numeric",
  gridOrthogonal="SpatialGrid",
  gridCartogram="data.frame",
  cartogram="SpatialPolygons"
  ), 
  ## The algorithm by Newman works best if there is a generous "sea" around
  ## the "land", thus By default, add 50% of the x/y ranges to each side and
  ## define a grid of 512x512 points.
  prototype(
  nrows=2^8,
  ncols=2^8,
  seaSize=0.5,
  variable=1
  ),
 package="cart",
 S3methods = FALSE)