stat_voronoi <- function(mapping = NULL, data = NULL, geom = "polygon",
                         position = "identity", na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, outline = NULL, ...) {
  layer(
    stat = StatVoronoi, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, outline = outline, ...)
  )
}