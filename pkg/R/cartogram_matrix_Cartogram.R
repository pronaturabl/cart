#' TODO: check that we have at least two columns in matrix
setMethod("cartogram",
    signature(x = "matrix", cart = "Cartogram"),
    function (x = "ANY", cart = "Cartogram", ...) 
    {
        cartogram.interpolateXYcoord(cartogram.shiftScaleCoords(x, cart),cart)
    }
)
