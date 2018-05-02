library(ggplot2)
library(deldir)
library(rgeos)
library(sp)
library(raster)

voronoi_polygon = function(x, y, fill = NULL,
                           outline.x=NULL,outline.y=NULL,outline.group=NULL) 
{
    if(is.null(outline.x) | is.null(outline.y)){
      outline=NULL
      }else{
        outline=data.frame(x=outline.x,y=outline.y)
        if(!is.null(outline.group)){
          outline$group=group
        }else{
          outline$group = rep(1, nrow(outline))
      }
    } 

if(!is.null(outline)){
    outline_polygons = lapply(unique(outline$group), function(group){
      poly = Polygon(outline[outline$group == group,1:2])
      Polygons(list(poly), ID = as.character(group))
    })
      outline_spdf = SpatialPolygonsDataFrame(SpatialPolygons(outline_polygons), 
                                              data = data.frame(group = unique(outline$group),
                                              row.names = unique(outline$group)))
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

circle = data.frame(x = 50*(1+cos(seq(0, 2*pi, length.out = 1000))), 
                    y = 50*(1+sin(seq(0, 2*pi, length.out = 1000))))
points = data.frame(x = sample(1:100, 100), y = sample(1:100, 100))
points$fill = sqrt((points$x-50)^2 + (points$y-50)^2)

vircle = voronoi_polygon(points$x,points$y,points$fill,
                         circle$x,circle$y)

ggplot()+geom_polygon(data=vircle,
                      aes(x=x,y=y,fill=fill,group=group))

ggplot()+geom_path(data=vircle,
                   aes(x=x,y=y,group=group))
