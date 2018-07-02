fortify_voronoi = function(vor_spdf,x="x",y="y"){
  data=vor_spdf@data
  data$id=rownames(vor_spdf@data)
  
  data$point.x=data$long
  data$point.y=data$lat
  data$long=NULL
  data$lat=NULL
  data$group=NULL
  
  voronoi = suppressMessages(ggplot2::fortify(vor_spdf, data)) 
  
  voronoi = left_join(voronoi,data,by="id")
  
  voronoi[,x]=voronoi$long
  voronoi[,y]=voronoi$lat
  voronoi$long=NULL
  voronoi$lat=NULL
  
  voronoi
}
