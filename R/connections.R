library(ggplot2)
library(grainscape)

library(stars)

mw_r <- read_stars('int_files/mixedWood.tif')

mw_r[mw_r==0] <- NA

mw_v <- st_as_sf(mw_r)

mw <- raster::raster('int_files/mixedWood.tif', downsample=100)



isBecomes <- cbind(c(1, 0), c(1, 3))
patchyCost <- raster::reclassify(mw, rcl = isBecomes)



# ggplot() +
#   geom_raster(data = ggGS(patchyCost),
#               aes(x = x, y = y, fill = value)) +
#   scale_fill_distiller(palette = "Paired", guide = "legend") +
#   guides(fill = guide_legend(title = "Resistance")) +
#   theme(legend.position = "right")

patchyMPG <- MPG(patchyCost, patch = (patchyCost == 1))
