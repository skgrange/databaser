#' Function to test database integrity. 
#' 
#' \code{db_integrity_check} is only implemented for SQLite databases. 
#' 
#' @param con Database connection. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_integrity_check <- function(con) {
  
  if (db.class(con) == "sqlite") {
    
    df <- db_get(con, "PRAGMA integrity_check")
    
  } else {
    
    stop("Not implemented...", call. = FALSE)
    
  }
    
  return(df)
  
}
