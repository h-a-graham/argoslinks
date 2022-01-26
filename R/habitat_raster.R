habitat_raster<- function(lcm, tcd, aoi, res, .dir){
  set_up_gdalio(aoi, res)
  
  lcm_gd <- gdalio_stars(lcm, resample="near", band_output_type='Int32')
  
  tcd_gd <- gdalio_stars(tcd, resample="cubicspline", band_output_type='Float64')
  gc()
  # convert tcd to binary woodland/nonwood
  tcd_gd[tcd_gd <25] <- 0
  tcd_gd[tcd_gd >=25 ] <- 1
  tcd_gd[is.na(lcm_gd)] <- NA
  
  # lcm_gd[lcm_gd!=1 | lcm_gd!=2] <- 0
  
  tcd_gd[lcm_gd==1] <- 1
  tcd_gd[lcm_gd==2] <- 2
  
  mwPath <- file.path(.dir, 'mixedWood.tif')
  stars::write_stars(tcd_gd, mwPath, type='Float32')
           
  tcd_gd[lcm_gd==2] <- 0
  
  blPath <- file.path(.dir, 'blWood.tif')
  stars::write_stars(tcd_gd, blPath, type='Float32')
  
  rm(tcd_gd, lcm_gd)
  gc()
  return(list(mixedWood = mwPath, brdleafWood = blPath))
}