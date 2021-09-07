library(grainscape)
library(raster)
library(ggplot2)
library(sf)
library(dplyr)

patchy <- raster(system.file("extdata/patchy.asc", package = "grainscape"))

## Create an is-becomes matrix for reclassification
isBecomes <- cbind(c(1, 2, 3, 4, 5), c(1, 10, 8, 3, 6))
patchyCost <- reclassify(patchy, rcl = isBecomes)

## Plot this raster using ggplot2 functionality
## and the default grainscape theme 
ggplot() +
  geom_raster(data = ggGS(patchyCost),
              aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(palette = "Paired", guide = "legend") +
  guides(fill = guide_legend(title = "Resistance")) +
  theme(legend.position = "right")

patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))

plot(patchyMPG, quick = "mpgPlot", theme = FALSE)

## Extract tabular node information using the graphdf() function
nodeTable <- graphdf(patchyMPG)[[1]]$v
nodeTable

## Extract tabular link information using the graphdf() function
linkTable <- graphdf(patchyMPG)[[1]]$e
linkTable

scalarAnalysis <- threshold(patchyMPG, nThresh = 5)
scalarAnalysis

scalarAnalysis <- threshold(patchyMPG, nThresh = 100)
ggplot(scalarAnalysis$summary, aes(x = maxLink, y = nComponents)) +
  geom_line(colour = "forestgreen") +
  xlab("Link Threshold (resistance units)") +
  ylab("Number of components") +
  scale_x_continuous(breaks = seq(0, 1000, by = 100)) +
  scale_y_continuous(breaks = 1:20) +
  theme_light() + theme(axis.title = element_text())


ggplot() +
  geom_raster(data = ggGS(patchyCost),
              aes(x = x, y = y, fill = value)) +
  scale_fill_distiller(palette = "Paired", guide = "legend") +
  geom_segment(data  = ggGS(patchyMPG, "links"),
               aes(x = x1, y = y1, xend = x2, yend = y2,
                   colour = lcpPerimWeight >= 300)) +
  scale_colour_manual(values = c("forestgreen", 'pink')) +
  geom_point(data = ggGS(patchyMPG, "nodes"), aes(x = x, y = y),
             colour = "darkgreen")



# make it and sf object....
sfPoints <- function(...){
  tibble(...) %>%
    mutate(geometry = st_sfc(st_point((c(.$x,.$y)), dim = 'XY')))%>%
    st_as_sf(sf_column_name='geometry')
}

sfLines <- function(...){
  tibble(...) %>%
    mutate(geom= st_sfc(st_linestring(matrix(c(.$x1, .$x2, .$y1, .$y2), 
                                                  ncol=2), dim = 'XY')))%>%
    st_as_sf(sf_column_name='geom')
}

t1 <- ggGS(patchyMPG, "nodes") %>%
  purrr::pmap_dfr(~sfPoints(...))

t2 <- ggGS(patchyMPG, "links") %>%
  purrr::pmap_dfr(~sfLines(...))


habPatches <- patchyCost
habPatches[habPatches!=1]<- NA
raster::plot(habPatches)

habPatches_sf <- stars::st_as_stars(habPatches) %>%
  st_as_sf( merge=T) %>%
  mutate(patchId = row_number())

t3 <- t2 %>%
  filter(lcpPerimWeight<250) %>%
  st_buffer(1) %>%
  st_union() %>%
  st_cast('POLYGON') %>%
  st_as_sf() %>%
  mutate(line_n = row_number())

t4 <- habPatches_sf %>%
  mutate(cluster = as.numeric(st_intersects(habPatches_sf, t3))) %>%
  mutate(cluster = ifelse(is.na(cluster),
                        row_number()+max(cluster, na.rm=T),
                        cluster)) %>%
  group_by(cluster) %>%
  summarise() %>%
  mutate(cluster = as.factor(row_number()))  %>%
  mutate(cluster_area = st_area(.))

ggplot()+
  geom_sf(data=t4, aes(fill=cluster_area, col=cluster), lwd=2)+
  geom_sf_label(data=t4, aes(label=cluster)) +
  geom_sf(data=st_geometry(st_convex_hull(t4)), fill=NA)+
  scale_fill_viridis_c(option='turbo') +
  scale_colour_brewer(palette='Dark2')



# minimal example.
tibble(x1=c(1,5),x2=c(2,3),y1=c(4,5),y2=c(5,6))%>%
  pmap_dfr(function(...){
    tibble(...)%>%
      mutate(g=st_sfc(st_linestring(matrix(c(.$x1, .$x2, .$y1, .$y2), 
                                           ncol=2), dim = 'XY')))%>%
      st_as_sf(sf_column_name='g')}) %>%
  ggplot() + geom_sf()
