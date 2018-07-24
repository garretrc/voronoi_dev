#' Invert a sp polygon
#' 
#' Warning! You are likely better off using the outline argument of voronoi_polygon/voronoi_path.
#' This function inverts a map you want to use voronoi_polygon() on to hide voronoi lines outside of the map.
#' @param map SpatialPolygonsDataFrame with lat and long
#' @param width mask border size
#' @param piece if your SPDF has pieces choose which one to mask
#' @keywords mask
#' @import ggplot2 
#' @export
#' @examples ggplot+geom_polygon(mask_map(map),aes(x=long,y=lat))
#' mask_map()

mask_map = function(map,width=5,piece=1){
  map = ggplot2::fortify(map)
  
  if(!is.null(map$piece)){
    map = map[which(map$piece==piece),]
  }
  
  top = max(map$lat) + width# north lat
  left = min(map$long) - width # west long
  right = max(map$long) + width # east long
  bottom = min(map$lat) - width# south lat
  mid = left+right / 2
  
  map = rbind(map, map[1,])
  rim = data.frame(long = c(left, right, right, left, left), lat = c(top, top, bottom, bottom, top) )
  
  mask <- as.data.frame(rbind(rim,data.frame(long=map$long,lat=map$lat)))
  mask
}
