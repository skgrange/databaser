#' Function to return PostGIS version information from an PostGIS enabled 
#' PostgreSQL database. 
#' 
#' @param con Database connection. 
#' 
#' @param full Should full version information be returned? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with the lenth of one. 
#' 
#' 
db_post_gis_version <- function(con, full = TRUE) {
  
  # To-do: check with a database without postgis
  
  if (db.class(con) == "postgres") {
    
    if (full) {
      
      x <- db_get(con, "SELECT PostGIS_full_version()")[, 1]
      
    } else {
      
      x <- db_get(con, "SELECT PostGIS_version()")[, 1]
      
    }
    
  } else {
    
    stop("Connection must be a PostgreSQL connection...", call. = FALSE)
    
  }
  
  return(x)
  
}
