library(ggplot2)
library(grainscape)
library(dplyr)
library(stars)
library(lconnect)
library(rmapshaper)
library(sf)
library(dplyr)
library(tictoc)
library(furrr)
library(sfheaders)
# read raster and remove 0 values.
mw_r <- read_stars('int_files/mixedWood.tif')

mw_r[mw_r==0] <- NA

# convert to sf object
mw_v <- st_as_sf(mw_r, merge=T)

# tidy up and filter by area (10 ha)
mw_10h <- mw_v %>% 
  mutate(area = as.numeric(st_area(mw_v))) %>%
  rename(habitat=1) %>%
  filter(area > 100000) %>%
  st_make_valid() 

# simplify feature and buffer - union and explode to reate connected habitat zones.
mw_s<- mw_10h %>%
  ms_simplify(keep = 0.2) %>%
  st_buffer(250) %>%
  st_union() %>%
  st_cast('POLYGON') %>%
  st_as_sf() %>%
  mutate(patch_id=row_number())


# join original feaures with connected habitat zones
mw_join <- st_join(mw, mw_s)

plot(st_geometry(mw_s), axes = TRUE)
plot(mw_join['patch_id'], pal = sf.colors(nrow(mw_s), categorical = TRUE), border = NA,  add=T)

# st_write(mw_join, 'patch_test.gpkg', delete_dsn =T)
# st_write(mw_s, 'patch_chunks.gpkg', delete_dsn =T)




# -------- grainscape approach ---------


mw <- raster::raster('int_files/mixedWood.tif', downsample=100)



isBecomes <- cbind(c(1, 0), c(1, 3))
patchyCost <- raster::reclassify(mw, rcl = isBecomes)

# ggplot() +
#   geom_raster(data = ggGS(patchyCost),
#               aes(x = x, y = y, fill = value)) +
#   scale_fill_distiller(palette = "Paired", guide = "legend") +
#   guides(fill = guide_legend(title = "Resistance")) +
#   theme(legend.position = "right")
tic()
patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))
message(toc())

saveRDS(patchyMPG, 'temp_storage/patchyMPG.rds')

plot(patchyMPG, quick = "mpgPlot", theme = FALSE)
nodeTable <- graphdf(patchyMPG)[[1]]$v
linkTable <- graphdf(patchyMPG)[[1]]$e



# sfLines <- function(...){
#   tibble(...) %>%
#     mutate(geometry= st_sfc(st_linestring(matrix(c(.$x1, .$x2, .$y1, .$y2), 
#                                              ncol=2), dim = 'XY')))%>%
#     st_as_sf(sf_column_name='geometry')
# }
# 
# 
# plan(multisession, workers = parallel::detectCores()-2)
# 
# t2 <- ggGS(patchyMPG, "links") %>%
#   furrr::future_pmap_dfr(~sfLines(...))

# ggGS(patchyMPG, "links")%>%
#   rowwise() %>%
#   mutate(geometry=st_sfc(st_linestring(matrix(c(x1, x2, y1, y2), 
#                                        ncol=2), dim = 'XY'))) %>%
#   st_as_sf(sf_column_name='geometry')%>%
#   ungroup()


t2 <- ggGS(patchyMPG, "links") %>%
  mutate(id=row_number())%>%
  pivot_longer(c(x1p,x2p,y1p,y2p),
               names_to = c(".value", "loc"),
               names_pattern = "(.)(.)") %>%
  sf_linestring(., x='x', y='y', linestring_id = "id", keep=T) %>%
  st_set_crs(st_crs(27700))
  #ggplot() + geom_sf()

st_write(t2, 'temp_storage/linksP.gpkg')

t2Points <- wk::wk_vertices(t2)


habPatches <- patchyCost
habPatches[habPatches!=1]<- NA
# raster::plot(habPatches)

habPatches_sf <- stars::st_as_stars(habPatches) %>%
  st_as_sf( merge=T) %>%
  mutate(patchId = row_number())

t3 <- t2 %>%
  filter(lcpPerimWeight<25) %>%
  st_buffer(1) %>%
  st_union() %>%
  st_cast('POLYGON') %>%
  st_as_sf() %>%
  mutate(line_n = row_number()) %>%
  st_set_crs(st_crs(habPatches_sf))

st_intersects(habPatches_sf, t2)
# Need to make a more efficient st_intersects...

st_intersects_para<- function(x,y){
  st_int_p <- function(...){
    st_intersects(...)
  }
    st_make_grid(t3) %>%
    st_as_sf() %>%
    mutate(id=row_number()) %>%
    purrr::pmap_dfr(st_int_p(., ))

}

st_make_grid(t3) %>%
  st_as_sf() %>%
  mutate(id=row_number()) %>%
  purrr::pmap_dfr()

t4 <- habPatches_sf %>%
  mutate(cluster = as.numeric(st_intersects(habPatches_sf, t3))) %>%
  mutate(cluster = ifelse(is.na(cluster),
                          row_number()+max(cluster, na.rm=T),
                          cluster)) %>%
  group_by(cluster) %>%
  summarise() %>%
  mutate(cluster = as.factor(row_number()))  %>%
  mutate(cluster_area = st_area(.))


st_write_sf(t4, 'temp_storage/grainscape_test1.gpkg' )

ggplot()+
  geom_sf(data=t4, aes(fill=cluster_area, col=cluster), lwd=2)+
  geom_sf_label(data=t4, aes(label=cluster)) +
  geom_sf(data=st_geometry(st_convex_hull(t4)), fill=NA)+
  scale_fill_viridis_c(option='turbo') +
  scale_colour_brewer(palette='Dark2')

