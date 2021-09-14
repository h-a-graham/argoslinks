# function to plot the habitat patches and roads - interactive viewer.
view_patches <- function(hab_patch, corn_roads){
  hab_patch <- read_sf(hab_patch[[1]]) %>%
    mutate(patch_idF = as.factor(patch_id))
  
  # hab_chunk <- read_sf(hab_patch[[2]]) %>%
  #   mutate(patch_idF = as.factor(patch_id))
  
  corn_roads <- read_sf(tar_read(cornish_roads)) %>%
    filter(roadFunction %in% c("A Road", "B Road"))
  
  tmap_mode("view")
  # tmap_options(max.categories = 685)
  tm_basemap(server='Esri.WorldImagery')+
    tm_shape(hab_patch) +
    tm_fill(col = "patch_idF", pal=random_palette(nrow(hab_patch)),legend.show = FALSE, alpha = 0.7) +
    tm_borders(lwd=0.3)+
    tm_shape(corn_roads) +
    tm_lines(col = "roadFunction", palette = c('red', 'blue'), title.col='Roads')
}

