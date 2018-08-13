#' Function to test a SQLite database's integrity. 
#' 
#' \code{db_check_integrity} is only implemented for SQLite databases. 
#' 
#' @param con Database connection. 
#' 
#' @return Character vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_check_integrity <- function(con) {
  
  if (db.class(con) == "sqlite") {
    
    x <- db_get(con, "PRAGMA integrity_check")[,]
    
  } else {
    
    warning("Not implemented...", call. = FALSE)
    x <- as.character()
    
  }
    
  return(x)
  
}
