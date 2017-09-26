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
#' @export
db_post_gis_version <- function(con, full = TRUE) {
  
  # Build query
  sql <- ifelse(full, "SELECT PostGIS_full_version()", "SELECT PostGIS_version()")
  
  if (db.class(con) == "postgres") {
    
    x <- db_get(con, sql, warn = FALSE)[, 1]
    
    # Not a true error
    if (is.null(x)) {
      
      message("PostGIS is not installed...")
      x <- FALSE
      
    }
    
  } else {
    
    stop("Connection must be a PostgreSQL connection...", call. = FALSE)
    
  }
  
  return(x)
  
}
