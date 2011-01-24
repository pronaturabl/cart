#' Note that the proj4string is simply copied from input to ouput
#' It's either that, or use the cart@spdf@proj4string, or check
#' to see whether they're the same and if not, do something more clever
#' than I (dave) know about...
setMethod("cartogram",
    signature(x = "SpatialPolygons", cart = "Cartogram"),
    function (x = "ANY", cart = "Cartogram", ...) 
    {
    # I'm trusting that because this is called by signature, I
    # don't have to re-check the classes of the parameters
        SpatialPolygons(
          cartogram.interpolatePolygons(x@polygons, cart),
          proj4string = x@proj4string
          )
    }
)
