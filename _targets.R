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
source('R/random_palette.R')
source('R/view_patches.R')
source('R/create_al_fgdb.R')

future::plan(future::multisession, workers = 2)
# ==== target options ====
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("sf", "plyr", "tidyverse", "purrr", "furrr", "curl", "zip", 
                            "terra", "raster", "fasterize", "gdalio",
                            "rmapshaper", "stars", "tmap")) # need to check these soon -prety sure not all needed now...

# ==== Define raw data locations: ====
# CEH landcover 2019 20m raster
ceh_lcm19 <-'data/vegetation/FME_346E606F_1626178964646_1149/data/643eb5a9-9707-4fbb-ae76-e8e53271d1a0/gb2019lcm20m.tif'
# Copernicus TCD 2018 Data Folder
cop_tcd18 <- 'data/vegetation/TCD_2018_010m_gb_03035_v020/DATA'
#counties data
#ditching ONS counties...it sucks
# ons_counties <- 'data/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC/Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp'
ordsurv_bounds <- 'data/bdline_gpkg_gb/data/bdline_gb.gpkg'
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
                          ordsurv_bounds,
                          open_roads))),
  # mosaic and warp TCD data
  tar_target(mosaic_tcd, 
             warp_tcd(cop_tcd18, inter_data_dir)),
  tar_target(select_aoi,
             get_cornwall(ordsurv_bounds)),
  tar_target(warp_to_region,
              habitat_raster(ceh_lcm19, mosaic_tcd, select_aoi, ras_res, 
                             inter_data_dir)),
  tar_target(cornish_roads,
             cornwall_roads(open_roads, select_aoi, inter_data_dir)),

  tar_target(Wood_patches,
             woodland_vector(hab_ras=warp_to_region$mixedWood, aoi=select_aoi, 
                             save_folder=inter_data_dir, prefix='MW')),
  tar_target(road_split_woods,
             road_patches(Wood_patches, select_aoi,  tar_read(cornish_roads), 
                          road_types=c("A Road", "B Road"),al_dir, 
                          prefix='MW', ncores=14)),
  tar_target(squirrel_chunks,
             release_patches(road_split_woods$int_road_path, 
                              road_split_woods$road_zones, 
                              zone_size = 1e4, patch_min = 5e3, 
                              gap_distance=51, al_dir, 
                              prefix='Squirrel', ncores=5)),
  tar_target(marten_chunks,
             release_patches(road_split_woods$int_road_path, 
                              road_split_woods$road_zones, 
                              zone_size = 2e5, patch_min = 2e5, 
                              gap_distance=1100, al_dir, 
                              prefix='Marten', ncores=5)),
  
  tar_target(wildcat,
             release_patches(road_split_woods$int_road_path, 
                              road_split_woods$road_zones, 
                              zone_size = 1e6, patch_min = 7e4, 
                              gap_distance=1430, al_dir, 
                              prefix='Wildcat', ncores=5)),
  tar_target(boar,
             release_patches(road_split_woods$int_road_path, 
                              road_split_woods$road_zones, 
                              zone_size = 1e6, patch_min = 25e4, 
                              gap_distance=2980, al_dir, 
                              prefix='Boar', ncores=5)),
  tar_target(AllWood_interactiveMap,
             view_patches(all_wood= road_split_woods$int_road_path,
                          corn_roads =cornish_roads, spc_name='AllWood')),
  tar_target(Boar_interactiveMap,
             view_patches(species_lyr= boar,
                          corn_roads =cornish_roads, spc_name='Boar')),
  tar_target(Squirrel_interactiveMap,
             view_patches(species_lyr= squirrel_chunks,
                          corn_roads =cornish_roads, spc_name='Squirrel')),
  tar_target(Marten_interactiveMap,
             view_patches(species_lyr= marten_chunks,
                          corn_roads =cornish_roads, spc_name='Marten')),
  tar_target(Wildcat_interactiveMap,
             view_patches(species_lyr= wildcat,
                          corn_roads =cornish_roads, spc_name='Wildcat')),
  tar_target(export_gpkg,
             create_al_fgdb(c(squirrel_chunks, marten_chunks, wildcat, boar),
                            file.path(al_dir, "ArgosLinks.gpkg")))
  
  
)
