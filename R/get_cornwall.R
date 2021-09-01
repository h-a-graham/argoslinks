get_cornwall <- function(counties){
  
  counties <- st_read(counties)
  
  corn <- counties %>%
    filter(ctyua19nm=='Cornwall')
  
  return(corn)
}