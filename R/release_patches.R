# select woodland chunks based on woodland patch area, gap distance, 
# road intersections.

woodland_vector <- function(hab_ras, aoi, save_folder, prefix){

  #convert rasters to sf features
  hab_r <- read_stars(hab_ras)
  
  hab_r[hab_r==0] <- NA
  
  hab_v <- st_as_sf(hab_r, merge=T) %>%
    st_make_valid() %>%
    st_intersection(st_buffer(aoi, 1000)) %>%
    mutate(wood_class = case_when(mixedWood.tif == 1 ~ "Broadleaf",
                                  mixedWood.tif == 2 ~ "Conifer")) %>%
    dplyr::select(wood_class)
  
  hab_v_reduce <- hab_v %>% 
    filter(st_geometry_type(.) %in% c('MULTIPOLYGON', 'POLYGON')) %>%
    st_cast('POLYGON') %>%
    mutate(area = as.numeric(st_area(hab_v))) %>%
    filter(area > 1000)
  
  hab_v_simp <- rmapshaper::ms_simplify(hab_v_reduce, keep=0.25)
  
  # s_name <- file.path(save_folder, sprintf('%s_WoodCover.gpkg', prefix))
  # st_write(hab_v, s_name, delete_dsn = TRUE)
  
  s_name2 <- file.path(save_folder, sprintf('%s_WoodCoverSimple025.gpkg', prefix))
  st_write(hab_v_simp, s_name2, delete_dsn = TRUE)
  
  return(s_name2)
}

split_by_roads <- function(feature, roads){
  lwgeom::st_split(feature, roads) %>%
    st_collection_extract(c("POLYGON"))  %>%
    st_make_valid() 
}

road_patches <- function(wood_cover, aoi,  corn_roads, 
                         road_types=c("A Road", "B Road"), save_folder, prefix,
                         ncores=4){
  
  # read wooland spatial vector 
  wc <- read_sf(wood_cover) %>%
    st_make_valid()
  
  # filter road types to consier
  corn_roads <- read_sf(corn_roads) %>%
    dplyr::filter(roadFunction %in% road_types)%>%
    st_union()
  
  r_patch_regions <- split_by_roads(st_buffer(aoi, -500), corn_roads) %>%
    mutate(id=row_number()) %>%
    dplyr::select(id)
  
  # get features that intersect the patches and get intersection
  road_inter_JOIN <-wc %>%
    st_filter(., st_union(r_patch_regions)) %>%
    st_intersection(., r_patch_regions)   %>%
    wk::wk_flatten(.) %>%
    filter(st_is(st_geometry(.), c("POLYGON")))%>%
    st_make_valid()

  # get features that cross or don't intersect and then bind them
  road_non_inter <- wc %>%
    st_filter(., st_union(r_patch_regions), .predicate = st_disjoint) %>%
    wk::wk_flatten(.)%>%
    filter(st_is(st_geometry(.), c("POLYGON"))) %>%
    st_make_valid() 
  road_non_cross<- wc %>%
    st_filter(., st_union(r_patch_regions), .predicate = st_overlaps) %>%
    wk::wk_flatten(.)%>%
    filter(st_is(st_geometry(.), c("POLYGON"))) %>%
    st_make_valid()
  road_non_bind <- road_non_inter %>%
    bind_rows(., road_non_cross) %>%
    st_difference(., st_union(r_patch_regions)) %>%
    wk::wk_flatten(.)%>%
    filter(st_is(st_geometry(.), c("POLYGON"))) %>%
    st_make_valid()
  
  # get nearest k neighbours join between zones and non intersecting/crossing 
  # features.
  road_NONinter_JOIN <- st_join(road_non_bind,r_patch_regions ,
                                join = nngeo::st_nn, maxdist = 750, k = 1, 
                                progress = TRUE, parallel=ncores) %>%
    tidyr::drop_na() %>%
    st_as_sf() %>%
    filter(st_is(st_geometry(.), c("POLYGON", "MULTIPOLYGON"))) %>%
    st_make_valid()
  
  # combine intersection and disjoint features.
  road_patches_edit <- road_inter_JOIN %>%
    bind_rows(road_NONinter_JOIN) %>%
    filter(st_is(st_geometry(.), c("POLYGON", "MULTIPOLYGON"))) %>%
    rename(inter_road_zone=id) %>%
    group_by(inter_road_zone, wood_class) %>%
    summarise() %>%
    wk::wk_flatten()
  # save file
  s_name <- file.path(save_folder, sprintf('%s_InterRoadCover.gpkg', prefix))
  st_write(road_patches_edit, s_name, delete_dsn = TRUE)
  return(list(int_road_path = s_name,
              road_zones = r_patch_regions))
  
}

create_sp_zone <- function(sdf, road_zones, gap_distance, 
                           zone_size, patch_min){
  zone <- sdf$inter_road_zone[1]
  
  # if all of zone already smaller than min size - remove it. 
  sdf1 <- sdf %>%
    dplyr::summarise() %>%
    dplyr::filter(as.numeric(st_area(.)) > zone_size)
  
  if (nrow(sdf1)<1) {
    message('sdf1 filter')
    return(NULL)
  }
  sdf1B <- sdf1 %>% 
    wk::wk_flatten() %>%
    dplyr::filter(as.numeric(st_area(.)) > patch_min)
  
  if (nrow(sdf1B)<1) {
    message('sdf1B filter')
    return(NULL)
  } 
    
     
  sdf2 <-  sdf1B %>%
    sf::st_buffer(gap_distance/2) %>% # divide by 2 because other woods are also buffered an this is the halfway point.
    dplyr::summarise() %>%
    wk::wk_flatten() %>%
    # sf::st_as_sf() %>% 
    sf::st_intersection(., dplyr::filter(road_zones,id==zone)) %>%
    sf::st_as_sf() 
  
  if (nrow(sdf2)<1) {
    message('sdf2 filter')
    return(NULL)
  }
  
  sdf3 <- sf::st_intersection(sdf1B, sdf2) 
  
  if (nrow(sdf3)<1) {
    message('sdf3A filter')
    return(NULL)
  }
  
  if (as.numeric(st_area(dplyr::summarise(sdf3))) < zone_size){
    message('sdf3B filter - zone < min zone area')
    return(NULL) 
  } 
    
  return(list(zones=sdf2, patches=sdf3))
}

release_patches <- function(roadPatches, road_zones, zone_size = 1e4, patch_min = 5e3, 
                            gap_distance=51, int_dir, prefix, ncores=4) {
  
  roadPatch_sf <- read_sf(roadPatches) #%>%
    # dplyr::filter(as.numeric(st_area(.)) > patch_min) ####
    
  
  patch_list <- roadPatch_sf %>%
    group_by(inter_road_zone) %>%
    group_split()
  

  plan(multisession,workers = ncores)
  ### CHANGE TO FURRR WHEN NOT DEBUGGING!
  proccesed_zones <- future_map(patch_list, ~create_sp_zone(.x, road_zones, gap_distance, 
                                               zone_size, patch_min))
  sp_zones <- purrr::map(proccesed_zones, 1) %>%
    bind_rows() %>%
    rename(inter_road_zone = id) %>%
    mutate(Area = st_area(.)) %>%
    dplyr::mutate(species_region=row_number()) %>%
    st_make_valid()
     
  mw_join <- purrr::map(proccesed_zones, 2) %>%
    bind_rows() %>%
    st_join(., sp_zones[c('species_region', 'inter_road_zone')],  
            largest=TRUE, join=st_overlaps)
  
  # mw_join <- st_join(roadPatch_sf, sp_zones['species_region'],  largest=TRUE,
  #                    join=st_overlaps) #join= st_is_within_distance, dist=750,
    
  mw_joinEDIT <- mw_join %>% dplyr::select(
                   inter_road_zone,
                   species_region) %>%
    group_by(species_region) %>%
    summarise() %>%
    mutate(Area = st_area(.))%>%
    tidyr::drop_na() %>%
      st_as_sf() %>%
    st_make_valid()
  
  hab_patch_path <- file.path(int_dir, sprintf('%s_hab_patches.gpkg', prefix))
  hab_chunk_path <- file.path(int_dir, sprintf('%s_hab_chunks.gpkg', prefix))
  st_write(mw_joinEDIT, hab_patch_path , delete_dsn =T)
  st_write(sp_zones, hab_chunk_path, delete_dsn =T)
  
  return(list(habitat_patches = hab_patch_path, habitat_chunks = hab_chunk_path))
}



