print.cartogram <- function(.Object,...) {
if (!class(.Object) == "Cartogram") {
    stop("argument '.Object' must be an object of class 'Cartogram'")      
      } 
    cat(sprintf("\nA Cartogram formed on a %dx%d grid", .Object@nrows, .Object@ncols))
    cat(sprintf("\nfrom %d polygons with bounding box {(%f,%f),(%f,%f)}"
      , length(.Object@spdf@polygons)
      , .Object@bbox[1,1], .Object@bbox[2,1]
      , .Object@bbox[1,2], .Object@bbox[2,2]
      ))
    cat("\n")
    } #function
  