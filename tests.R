library(sf)
library(dplyr)
library(leaflet)
library(targets)
library(gdalio)
library(terra)
library(raster)
counties <- st_read('data/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp')
ceh_lcm19 <-'data/vegetation/FME_346E606F_1626178964646_1149/data/643eb5a9-9707-4fbb-ae76-e8e53271d1a0/gb2019lcm20m.tif'
corn <- counties %>%
  filter(ctyua19nm=='Cornwall')

# plot(st_geometry(corn))

inter_data_dir <- file.path(here::here(),'int_files')
if (!dir.exists(inter_data_dir)) dir.create(inter_data_dir)

# m <- leaflet() %>%
#   addTiles() %>%
#   addPolygons(data=st_transform(corn, crs=4326))
# m

source('R/set_up_gdalio.R')
source('R/habitat_raster.R')

# memory leaks... why?!

habitat_raster(ceh_lcm19, tar_read(mosaic_tcd), tar_read(select_aoi), 10, inter_data_dir)




print('')
gc()

set_up_gdalio(corn, 10)

t1 <- gdalio_terra(ceh_lcm19)
terra()
plot(t1)

t2 <- gdalio_matrix(ceh_lcm19, anti_rotate = F)
t3 <- matrix_to_terra(t2)
t2

temp_save <- function(r){
  t <- tempfile(fileext = '.tif')
  terra::writeRaster(r, t)
}
