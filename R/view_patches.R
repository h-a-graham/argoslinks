# function to plot the habitat patches and roads - interactive viewer.
view_patches <- function(all_wood = NULL, species_lyr=NULL, corn_roads, 
                         spc_name='Species'){
  tmap_options(check.and.fix = TRUE)
  
  if (!is.null(all_wood)){
    hab_patch <- read_sf(all_wood) %>%
      mutate(Inter_Road_Zone = as.factor(inter_road_zone))
  }

  
  prep_spc <- function(path, item=1){
    read_sf(path[[item]]) %>%
      mutate(species_region = as.factor(species_region)) %>%
      mutate(Area_hec = Area/10000)
  }
  
  if (!is.null(species_lyr)){
    spc_patches <- prep_spc(species_lyr, item=1)
    
    spc_chunks <- prep_spc(species_lyr, item=2)
  }

  
  corn_roads <- read_sf(corn_roads) %>%
    filter(roadFunction %in% c("A Road", "B Road"))
  
  
  # tmap_mode("view")
  
  tm <- tm_basemap(server=c('Stamen.TonerLite','OpenStreetMap.Mapnik','Esri.WorldImagery', 'Off'))
  if (!is.null(all_wood)){
    tm <- tm + tm_shape(hab_patch, name='Woodland Type') +
      tm_polygons(col = "wood_class", pal=c('#1b9e77', '#d95f02'),legend.show = TRUE, alpha = 0.7,
                  title='Woodland Type', lwd=0.2) +
      
      tm_shape(hab_patch, name='Inter-Road Zones') +
      tm_polygons(col = "Inter_Road_Zone", pal=random_palette(nrow(hab_patch)),
                  legend.show = FALSE, alpha = 0.7, lwd=0.2)
  }
  if (!is.null(species_lyr)){
    tmap_options(max.categories = nrow(spc_chunks))
    cp <- random_palette(nrow(spc_chunks))
    tm <- tm  +
      tm_shape(spc_patches, name=sprintf('%s Woodland', spc_name)) +
      tm_polygons(col = "species_region", pal=cp,
                  legend.show = FALSE, alpha = 0.7, lwd=0.2) +

      tm_shape(spc_patches, name=sprintf('%s Woodland Area', spc_name)) +
      tm_polygons(col = "Area_hec", pal=viridisLite::viridis(n=256),
                  legend.show = TRUE, alpha = 0.7, lwd=0.2, 
                  title=sprintf('%s Zone Area  (ha)', spc_name)) +
      
      tm_shape(spc_chunks, name=sprintf('%s Zones', spc_name)) +
      tm_polygons(col = "species_region", pal=cp,
                  legend.show = FALSE, alpha = 0.7, lwd=0.2)
  }
  
  tm <- tm +
    tm_shape(corn_roads, name="A and B Roads") +
    tm_lines(col = "roadFunction", palette = c('black', 'grey30'), title.col='Road Network', lwd=2)
  
  # tm <- tm %>% 
  #   tmap_leaflet() %>%
  #   leaflet::hideGroup(c("Inter-Road Zones", 'Boar Woodland','Boar Zones',
  #                        'Boar Woodland Area', 'Squirrel Woodland',
  #                        'Pine marten Woodland','Wildcat Woodland', "corn_roads"))
  
  # htmlwidgets::saveWidget(tm, 'dev_maps/argosLinkTEST2.html', selfcontained = FALSE)
  tmap_save(tm, sprintf('dev_maps/%sArgosPatch_InterMap.html', spc_name))
  # tmap_save(tm, )
  
}

