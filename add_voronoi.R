add_voronoi = function (p, x = NULL, y = NULL, outline = NULL, markers = TRUE, ..., data = NULL, inherit = TRUE) 
{
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    data = data %||% p$x$visdat[[1]]()
  }
  if (is.null(x) || is.null(y)) {
    stop("Must supply `x`/`y` attributes", call. = FALSE)
  }
  if(class(x) == 'formula' & class(y) == 'formula'){
    voronoi = voronoi_polygon_2(data, attr(terms(x), 'term.labels'), attr(terms(y), 'term.labels'), 
                                outline) %>% group_by(group)  
  }else{
    voronoi = voronoi_polygon_2(data.frame(x, y), outline = outline) %>% group_by(group)
  }
  
  add_polygons(p,  fill = 'toself', hoveron = 'fills', ..., data = voronoi) %>% add_data(data)
}
