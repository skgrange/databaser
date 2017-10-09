#' Function to test database integrity. 
#' 
#' \code{db_check_integrity} is only implemented for SQLite databases. 
#' 
#' @param con Database connection. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_check_integrity <- function(con) {
  
  if (db.class(con) == "sqlite") {
    
    df <- db_get(con, "PRAGMA integrity_check")
    
  } else {
    
    stop("Not implemented...", call. = FALSE)
    
  }
    
  return(df)
  
}
