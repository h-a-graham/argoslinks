get_cornwall <- function(os_bounds){
  
  st_read(os_bounds,
          query = "SELECT * FROM 'boundary_line_ceremonial_counties' 
                   WHERE NAME = 'Cornwall'")
  
}