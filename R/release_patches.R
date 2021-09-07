# select woodland chunks based on woodland patch area, gap distance, 
# road intersections.

release_patches <- function(hab_ras, patch_min = 100000, gap_distance=250, aoi, 
                            corn_roads, road_types=c("A Road", "B Road"), 
                            int_dir, prefix) {
  # filter road types to consier
  corn_roads <- read_sf(corn_roads) %>%
    filter(roadFunction %in% road_types)%>%
    st_union()
  
  #convert rasters to sf features
  hab_r <- read_stars(hab_ras)
  
  hab_r[hab_r==0] <- NA
  
  hab_v <- st_as_sf(hab_r, merge=T)
  
  # tidy up and filter by min patch area area 
  hab_filt <- hab_v %>% 
    mutate(area = as.numeric(st_area(hab_v))) %>%
    dplyr::rename(habitat=1) %>%
    filter(area > patch_min) %>%
    st_make_valid() %>%
    st_intersection(st_buffer(aoi, 1000)) %>%
    st_cast('POLYGON')
     
  
  
  split_by_roads <- function(patches){
    lwgeom::st_split(patches, corn_roads) %>%
      st_collection_extract(c("POLYGON"))  %>%
      st_make_valid() 
  }
  
  roadPatches <- split_by_roads(hab_filt)
  
  
  mw_s<- roadPatches %>%
    ms_simplify(keep = 0.2) %>%
    st_buffer(gap_distance) %>%
    st_union() %>%
    st_cast('POLYGON') %>%
    st_as_sf() %>%
    split_by_roads() %>%
    mutate(patch_id=row_number()) 
  
  
  # plot(st_geometry(mw_s), col = sf.colors(12, categorical = TRUE), border = 'grey', 
  #      axes = TRUE)
  
  mw_join <- st_join(roadPatches, mw_s, join=st_intersects, largest=T) %>%
    dplyr::group_by(patch_id) %>%
    dplyr::summarise()
  
  
  # plot(st_geometry(mw_s), axes = TRUE)
  
  # plot(mw_join['patch_id'], pal = sf.colors(nrow(mw_s), categorical = TRUE), border = NA,  add=T)
  
  hab_patch_path <- file.path(int_dir, sprintf('%s_hab_patches.gpkg', prefix))
  hab_chunk_path <- file.path(int_dir, sprintf('%s_hab_chunks.gpkg', prefix))
  st_write(mw_join, hab_patch_path , delete_dsn =T)
  st_write(mw_s, hab_chunk_path, delete_dsn =T)
  
  return(list(habitat_patches = hab_patch_path, habitat_chunks = hab_chunk_path))
}



