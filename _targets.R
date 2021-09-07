#  ==== packages for the targets script ====
library(targets)
suppressMessages(library(here))

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


future::plan(future::multisession, workers = 2)
# ==== target options ====
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("sf", "plyr", "tidyverse", "purrr", "furrr", "curl", "zip", 
                            "terra", "raster", "fasterize", "gdalio",
                            "rmapshaper", "stars")) # need to check these soon -prety sure not all needed now...

# ==== Define raw data locations: ====
# CEH landcover 2019 20m raster
ceh_lcm19 <-'data/vegetation/FME_346E606F_1626178964646_1149/data/643eb5a9-9707-4fbb-ae76-e8e53271d1a0/gb2019lcm20m.tif'
# Copernicus TCD 2018 Data Folder
cop_tcd18 <- 'data/vegetation/TCD_2018_010m_gb_03035_v020/DATA'
#counties data
ons_counties <- 'data/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp'
# OS open roads
open_roads <- 'data/oproad_gpkg_gb/data/oproad_gb.gpkg'


# # National Forest Inventory 2018 spatial vector (zipped shp format)
# nfi_2018 <- 'data/vegetation/National_Forest_Inventory_Woodland_GB_2018-shp/8257a753-353e-48a5-8a6e-d69e63121aa5202041-1-1kunv01.h8eo.shp'
# # OS VectorMapDistrict 
# os_vmd <- 'data/vegetation/VectorMapDistrict/data/vmdvec_gb.gpkg'
# # OS MAsterMapRivers (CLOSED DATA.)
# os_mm_rivnet <- 'data/river_nets/os_MasterMap_Rivers/OS_MM_rivnet.gpkg'

# ==== desired resoltuion =====

ras_res <- 10


# ==== key folder locations ======
inter_data_dir <- file.path(here(),'int_files')
if (!dir.exists(inter_data_dir)) dir.create(inter_data_dir)

al_dir <- file.path(here(),'argos_links_out')
if (!dir.exists(al_dir)) dir.create(al_dir)

# ==== Target list ====
list(
  tar_target(data_check, 
             check_data(c(ceh_lcm=ceh_lcm19,
                          cop_tcd=cop_tcd18,
                          ons_counties,
                          open_roads))),
  # mosaic and warp TCD data
  tar_target(mosaic_tcd, 
             warp_tcd(cop_tcd18, inter_data_dir)),
  tar_target(select_aoi,
             get_cornwall(ons_counties)),
  tar_target(warp_to_region,
              habitat_raster(ceh_lcm19, mosaic_tcd, select_aoi, ras_res, 
                             inter_data_dir)),
  tar_target(cornish_roads,
             cornwall_roads(open_roads, select_aoi, inter_data_dir)),
  tar_target(mix_Wood_R_patches,
             release_patches(warp_to_region$mixedWood, patch_min = 100000, 
                             gap_distance=250, select_aoi, 
                             cornish_roads, 
                             road_types=c("A Road", "B Road"),
                             inter_data_dir, prefix='MW')),
  tar_target(broad_Wood_R_patches,
             release_patches(warp_to_region$brdleafWood, patch_min = 100000, 
                             gap_distance=250, select_aoi, 
                             cornish_roads, 
                             road_types=c("A Road", "B Road"),
                             inter_data_dir, prefix='BW'))
  
)
