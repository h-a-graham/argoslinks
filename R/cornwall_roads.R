cornwall_roads <- function(open_roads_path, aoi, int_dir){
  corn_roads <- read_sf('data/oproad_gpkg_gb/data/oproad_gb.gpkg', layer='RoadLink') %>%
    st_intersection(., st_buffer(aoi, 1000)) %>%
    st_make_valid()
  
  out_path <- file.path(int_dir, 'cornish_roads.gpkg')
  
  st_write(corn_roads, out_path, delete_dsn = T)
  
  return(out_path)
}