cartogram.interpolatePolygons <- function (polygons, Cartogram) {

  ## check arguments
if (!class(Cartogram) == "Cartogram") {
    stop("argument 'Cartogram' must be an object of class 'Cartogram'")      
      } 

  ## polygons
  ## @todo

  ## grid
  ## - must have class SpatialGrid
  if (!class(Cartogram@gridOrthogonal) == "SpatialGrid")
    stop("Cartogram@gridOrthogonal must be an object of class 'SpatialGrid'")
 
  ## Cartogram@gridCartogram is actually just a table
 
  bb <-Cartogram@bbox
  dim <- c(x = Cartogram@ncols, y = Cartogram@nrows) #rename? there is a base function dim
  range <- diff(t(bb)) #had been a second call to bbox
  shift <- Cartogram@seaSize * range

  ## loop over all polygons
  PolygonsList <- list()
  for (i in seq(along = polygons)) {
    PolygonList <- list()
    for (j in seq(along = polygons[[i]]@Polygons)) {
      Polygons <- polygons[[i]]@Polygons[[j]]
      coords <- Polygons@coords

      ## shift and scale coordinates according to grid
      coords <- cartogram.shiftScaleCoords(coords,Cartogram)

      ## interpolate
      coordsTr <- cartogram.interpolateXYcoord(coords, Cartogram)
      PolygonList[[j]] <- Polygon(coordsTr, hole = FALSE)
    }
    ID <- polygons[[i]]@ID
    PolygonsList[[i]] <- Polygons(PolygonList, ID)
  }
  #return:
  SpatialPolygons(PolygonsList, proj4string = Cartogram@spdf@proj4string)
}