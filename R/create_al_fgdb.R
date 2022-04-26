
create_al_fgdb <- function(gpkg_list, fgb_name){
  
  if (file.exists(fgb_name)) file.remove(fgb_name)
  
  make_fgdb <- function(x){
    type_name <- gsub(".*[_]([^.]+)[.].*", "\\1", x)
    sp_name <- gsub( "_.*$", "", x)
    
    lyr_name <- paste0(type_name, sp_name)
    
    ply <- sf::read_sf(x)
    sf::write_sf(ply, fgb_name, layer = lyr_name)
    
  }
  
  ply_lyrs <- gpkg_list %>%
    purrr::map(.x=., ~make_fgdb(.x))
  
}