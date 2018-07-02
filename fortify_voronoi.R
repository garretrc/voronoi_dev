fortify_voronoi = function(vor_spdf){
  data=vor_spdf@data
  data$id=rownames(vor_spdf@data)
  
  data$point.x=data[,1]
  data$point.y=data[,2]
  data[,1:2]=NULL
  data$group=NULL
  
  voronoi = suppressMessages(ggplot2::fortify(vor_spdf, data)) 
  
  voronoi = left_join(voronoi,data,by="id")
  
  voronoi$x=voronoi$long
  voronoi$y=voronoi$lat
  voronoi$long=NULL
  voronoi$lat=NULL
  
  voronoi
}
