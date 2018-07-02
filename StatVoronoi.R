StatVoronoi <- ggproto("StatVoronoi", Stat, 
                       required_aes = c("x", "y"),
                       
                       compute_group = function(data, scales, outline = NULL) {
                         voronoi_polygon(data$x, data$y, data$fill, outline,data.frame=T)
                       }
)
