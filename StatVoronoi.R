StatVoronoi <- ggproto("StatVoronoi", Stat, 
                       required_aes = c("x", "y"),
                       
                       compute_group = function(data, scales, outline = NULL) {
                         voronoi_polygon(data, "x", "y",outline= outline,data.frame=TRUE)
                       }
)
