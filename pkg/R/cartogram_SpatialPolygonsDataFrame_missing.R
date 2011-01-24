#' The original cartogram function took a SpatialPolygonsDataFrame
#' and returned a SpatialPolygons list; this emulates that behavior
setMethod("cartogram",
    signature(x = "SpatialPolygonsDataFrame", cart = "missing"),
    function (x = "ANY", cart = "Cartogram", ...) 
    {
        myC <- new("Cartogram",spdf=x,...)
        myC@cartogram
    }
)
