
create_al_fgdb <- function(gpkg_list, f_name){
  
  if (file.exists(f_name)) {
    file.remove(f_name) 
    message('old version deleted...')
  } 
  
  make_fgdb <- function(x){
    type_name <- gsub(".*[_]([^.]+)[.].*", "\\1", x)
    sp_name <- gsub( "_.*$", "", basename(x))
    
    lyr_name <- paste(sp_name, type_name, sep="_")
    
    ply <- sf::read_sf(x)
    sf::write_sf(ply, f_name, layer = lyr_name)
    
  }
  
  ply_lyrs <- gpkg_list %>%
    purrr::map(.x=., ~make_fgdb(.x))
  
  return(f_name)
  
}