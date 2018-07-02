StatVoronoi <- ggproto("StatVoronoi", Stat, 
                       required_aes = c("x", "y"),
                       
                       compute_group = function(data, scales, outline = NULL) {
                         voronoi_polygon(data, deparse(substitute(x)), deparse(substitute(y)),outline= outline,data.frame=TRUE)
                       }
)
