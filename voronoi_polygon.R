voronoi_polygon = function(x, y, fill = NULL, outline = NULL) 
{
  if(!is.null(outline)){
    if(class(outline) != "data.frame" & class(outline) != "SpatialPolygonsDataFrame"){
      outline = NULL
      warning("Outline must be of class data.frame or SpatialPolygonsDataFrame. No outline will be used.")
    }
    else if(class(outline) == "data.frame"){
      if(!is.numeric(outline[,1]) | !is.numeric(outline[,2])){
        warning("Columns 1 and 2 of Outline must be numeric. No outline will be used.")
        break
      }
      if(is.null(outline$group)){
        outline$group = rep(1, nrow(outline))
      }
      outline_polygons = lapply(unique(outline$group), function(group){
        poly = Polygon(outline[outline$group == group,1:2])
        Polygons(list(poly), ID = as.character(group))
      })
      outline_spdf = SpatialPolygonsDataFrame(SpatialPolygons(outline_polygons), 
                                              data = data.frame(group = unique(outline$group),
                                                                row.names = unique(outline$group)))
    }
  }
  data = data.frame(x, y)
  data$fill = fill
  pts = SpatialPointsDataFrame(cbind(x, y), data, match.ID = T)
  vor_desc = tile.list(deldir(pts@coords[, 1], pts@coords[, 2]))
  vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
    tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
    tmp <- rbind(tmp, tmp[1, ])
    Polygons(list(Polygon(tmp)), ID = i)
  })
  rownames(pts@data) = sapply(slot(SpatialPolygons(vor_polygons), 
                                   "polygons"), slot, "ID")
  vor_spdf = SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), 
                                      data = pts@data)
  if(!is.null(outline)){
    vor_spdf = intersect(gBuffer(vor_spdf, byid=TRUE, width=0), gBuffer(outline_spdf, byid=TRUE, width=0))
  }
  
  voronoi = suppressMessages(ggplot2::fortify(vor_spdf)) 
  
  voronoi$fill = unlist(lapply(1:length(vor_spdf@polygons), function(i){
    unlist(lapply(1:length(slot(vor_spdf@polygons[[i]], 'Polygons')), function(j){
      rep(vor_spdf@data$fill[i], nrow(slot(slot(vor_spdf@polygons[[i]], 'Polygons')[[j]], 'coords')))
    }))
  }))
  names(voronoi) = c('x', 'y', names(voronoi)[3:ncol(voronoi)])
  return(voronoi)
}