#' \code{cartogram()} will transform polygons and points according to a Cartogram
#' this doesn't go here, but...
#' @examples
#' #sort of a vignette
#' data(usapop) #from Thomas Zumbrunn's cart
#' usmap <- map("state", fill = TRUE, plot = FALSE)
#' sp <- map2SpatialPolygons(usmap, sub(":.*", "", usmap$names))
#' rownames(usapop) <- tolower(rownames(usapop))
#' spdf <- SpatialPolygonsDataFrame(sp, usapop, match.ID = TRUE)
#' #cart <- cartogram(spdf, "population") #original
#' myC <- new("Cartogram",spdf)
#' 
#' #plot(cart, axes = TRUE, asp = 1/2, col = "#147f14")
#' plot(myC, axes = TRUE, asp = 1/2, col = "#147f14")
#' #http://toolserver.org/~geohack/geohack.php?pagename=Washington,_D.C.&params=38_53_42.4_N_77_02_12.0_W_type:city_region:US-DC
#' #note that they give (lat,lon) (y,x) as 38.895111, -77.036667
#' #we have to swap order to match other data
#' washingtonDc <- matrix(c(-77.036667, 38.895111), ncol=2)
#' washPoint<-cartogram.interpolateXYcoord(cartogram.shiftScaleCoords(washingtonDc, myC),myC)
#' op <- par(new=TRUE) #overlay Washington DC:
#' points(washPoint, pch=18, col="red")
#' par(op) #reset graphics parameters (no overlay)
cartogram <- function(x="ANY",cart="Cartogram",...){standardGeneric("cartogram")}
setGeneric("cartogram")