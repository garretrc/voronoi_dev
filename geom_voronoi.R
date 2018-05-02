geom_voronoi = function (mapping = NULL, data = NULL, stat = StatVoronoi, position = "identity", 
                         ...,na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, outline = NULL) 
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, outline = outline,...))
}