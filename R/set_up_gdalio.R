set_up_gdalio <- function(aoi, res){
  
  bounds <- aoi %>%
    st_bbox() %>%
    as.list() %>%
    purrr::map(., ~plyr::round_any(., res))
  
  grid0 <- list(extent = c(bounds$xmin, 
                           bounds$xmax, 
                           bounds$ymin, 
                           bounds$ymax), 
                dimension = c((bounds$xmax-bounds$xmin)/res,
                              (bounds$ymax-bounds$ymin)/res), 
                projection = st_crs(aoi)$wkt)
  gdalio_set_default_grid(grid0)
  
}


# matrix_to_terra <- function(.matrix){
#   g <- gdalio_get_default_grid()
#   r <- terra::rast(terra::ext(g$extent), nrows = g$dimension[2], ncols = g$dimension[1], crs = g$projection)
#   terra::setValues(r, .matrix)
#   r
# }

# matrix_to_raster <- function(.matrix) {
#     g <- gdalio_get_default_grid()
#     r <- raster::raster(raster::extent(g$extent), nrows = g$dimension[2], ncols = g$dimension[1], crs = g$projection)
#     
#     t <- raster::setValues(r, .matrix) # this is super lazy but I couldn't get raster to create the right dims so...
#     raster::raster(t)
#   }
# 
# 
# gdalio_matrix <- function(dsn, anti_rotate=TRUE, ...) {
#   v <- gdalio_data(dsn, ...)
#   g <- gdalio_get_default_grid()
#   
#   m <- matrix(v[[1]], g$dimension[1])[,g$dimension[2]:1, drop = FALSE]
#   
#   if (isTRUE(anti_rotate)){
#      return(apply(t(m),2,rev))
#   } else {
#     return(m)
#   }
#   
# }

# vector_to_terra <- function(v, ...) {
#   v <- list(band1=v)
#   g <- gdalio_get_default_grid()
#   r <- terra::rast(terra::ext(g$extent), nrows = g$dimension[2], ncols = g$dimension[1], crs = g$projection)
#   if (length(v) > 1) terra::nlyr(r) <- length(v)
#   r <- terra::setValues(r, do.call(cbind, v))
#   return(r)
# }


## ---- gdalio_raster
## {raster}
# gdalio_raster <-
#   function(dsn, ...) {
#     v <- gdalio_data(dsn, ...)
#     g <- gdalio_get_default_grid()
#     r <- raster::raster(raster::extent(g$extent), nrows = g$dimension[2], ncols = g$dimension[1], crs = g$projection)
#     if (length(v) > 1) {
#       r <- raster::brick(replicate(length(v), r, simplify = FALSE))
#     }
#     raster::setValues(r, do.call(cbind, v))
#   }


## ---- gdalio_terra
## {terra}
# gdalio_terra <- function(dsn, ...) {
#   v <- gdalio_data(dsn, ...)
#   g <- gdalio_get_default_grid()
#   r <- terra::rast(terra::ext(g$extent), nrows = g$dimension[2], ncols = g$dimension[1], crs = g$projection)
#   if (length(v) > 1) terra::nlyr(r) <- length(v)
#   terra::setValues(r, do.call(cbind, v))
# }

# ---- gdalio_stars
## {stars}
gdalio_stars <- function(dsn, ...) {
  v <- gdalio_data(dsn, ...)
  g <- gdalio_get_default_grid()
  aa <- array((unlist(v, use.names = FALSE)), c(g$dimension[1], g$dimension[2], length(v)))#[,g$dimension[2]:1, , drop = FALSE]
  if (length(v) == 1) aa <- aa[,,1, drop = TRUE]
  r <- stars::st_as_stars(sf::st_bbox(c(xmin = g$extent[1], ymin = g$extent[3], xmax = g$extent[2], ymax = g$extent[4])),
                          nx = g$dimension[1], ny = g$dimension[2], values = aa)
  
  r <- sf::st_set_crs(r, g$projection)
  r

}
