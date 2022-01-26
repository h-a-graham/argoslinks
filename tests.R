library(sf)
library(plyr)
library(dplyr)
library(purrr)
library(furrr)
library(curl)
library(zip)
library(fasterize)
library(rmapshaper)
library(stars)
library(tmap)
library(leaflet)
library(targets)
library(gdalio)
library(terra)
library(raster)
library(here)
library(ggplot2)
LMS <- function(){
  # ==== external functions to import ====
  source('R/check_data.R')
  # source('R/get_OS_grid.R')
  source('R/warp_tcd.R')
  source('R/warp_method.R')
  source('R/set_up_gdalio.R')
  source('R/get_cornwall.R')
  source('R/cornwall_roads.R')
  source('R/habitat_raster.R')
  source('R/release_patches.R')
  source('R/random_palette.R')
  source('R/view_patches.R')
  source('R/get_cornwall.R')
}
LMS()

ordsurv_bounds <- 'data/bdline_gpkg_gb/data/bdline_gb.gpkg'
corn <- get_cornwall(ordsurv_bounds)

# inter_data_dir <- file.path(here(),'int_files')
blf <- tar_read(warp_to_region)

ceh_lcm19 <-'data/vegetation/FME_346E606F_1626178964646_1149/data/643eb5a9-9707-4fbb-ae76-e8e53271d1a0/gb2019lcm20m.tif'
mosaic_tcd <- tar_read('mosaic_tcd')
select_aoi <- tar_read('select_aoi')
ras_res <-10
inter_data_dir <- 'temp_storage'

habitat_raster(ceh_lcm19, mosaic_tcd, select_aoi, ras_res, 
               inter_data_dir)



wc <- woodland_vector(hab_ras="temp_storage/mixedWood.tif", aoi=corn, 
                      save_folder='temp_storage', prefix='MW')

# 
# corn_roads <- read_sf(tar_read(cornish_roads)) %>%
#   dplyr::filter(roadFunction %in% c("A Road", "B Road"))%>%
#   st_union() %>%
#   st_as_sf()
# 
# sf::st_geometry(corn_roads) <- st_extend_line(sf::st_geometry(corn_roads), 1000)
# 
# v <- st_cast(corn, 'MULTILINESTRING')
# v2 <- st_line_merge(corn_roads)
# plot(v2)
# plot(st_geometry(corn_roads), add=TRUE)

rp <- road_patches('temp_storage/MW_WoodCoverSimple025.gpkg', corn,  tar_read(cornish_roads), 
             road_types=c("A Road", "B Road"),'temp_storage', prefix='TEST', ncores=16)


squirrel_chunks <- release_patches (rp$int_road_path, rp$road_zones, 
                                    zone_size = 1e4, patch_min = 5e3, 
                                    gap_distance=51, 'temp_storage', 
                                    prefix='TEST', ncores = 16)



view_patches(all_wood= tar_read(road_split_woods)$int_road_path, 
             corn_roads =tar_read(cornish_roads), spc_name='AllWood')
view_patches(species_lyr= tar_read(boar), corn_roads =tar_read(cornish_roads), spc_name='Boar')






# ------- JUNK -----------------
# release_patches(blf$brdleafWood, patch_min = 50000, 
#                 gap_distance=500, corn, 
#                 tar_read(cornish_roads), 
#                 road_types=c("A Road", "B Road"),
#                 inter_data_dir, prefix='BW')
# convex_hullT <- read_sf(squirrel_chunks$habitat_patches) %>%
#   group_by(species_region ) %>%
#   dplyr::summarise() %>%
#   st_convex_hull() 

# 
# 
# 
# library(rnaturalearth)
# 
# uk_area <- rnaturalearth::ne_countries(scale=10, country='United Kingdom', returnclass = 'sf') %>%
#   st_transform(27700)
# 
# st_write(uk_area, 'temp_storage/ukAREA.gpkg', delete_dsn = TRUE)
# 
# counties <- st_read('data/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp')
# ceh_lcm19 <-'data/vegetation/FME_346E606F_1626178964646_1149/data/643eb5a9-9707-4fbb-ae76-e8e53271d1a0/gb2019lcm20m.tif'
# corn <- counties %>%
#   filter(ctyua19nm=='Cornwall')
# 
# # plot(st_geometry(corn))
# 
# inter_data_dir <- file.path(here::here(),'int_files')
# if (!dir.exists(inter_data_dir)) dir.create(inter_data_dir)
# 
# # m <- leaflet() %>%
# #   addTiles() %>%
# #   addPolygons(data=st_transform(corn, crs=4326))
# # m
# 
# source('R/set_up_gdalio.R')
# source('R/habitat_raster.R')
# 
# # memory leaks... why?!
# 
# habitat_raster(ceh_lcm19, tar_read(mosaic_tcd), tar_read(select_aoi), 10, inter_data_dir)
# 
# 
# 
# 
# print('')
# gc()
# 
# set_up_gdalio(corn, 10)
# 
# t1 <- gdalio_terra(ceh_lcm19)
# terra()
# plot(t1)
# 
# t2 <- gdalio_matrix(ceh_lcm19, anti_rotate = F)
# t3 <- matrix_to_terra(t2)
# t2
# 
# temp_save <- function(r){
#   t <- tempfile(fileext = '.tif')
#   terra::writeRaster(r, t)
# }
# 
# source('R/release_patches.R')
# release_patches('int_files/mixedWood.tif', patch_min = 100000, gap_distance=250, tar_read(select_aoi), 
#                 tar_read(cornish_roads), road_types=c("A Road", "B Road"),
#                 inter_data_dir) 
# 
