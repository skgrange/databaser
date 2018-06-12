#' Function to get SQLite \code{sqlite_master} query. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' 
#' @return Data frame. 
#' 
#' @export
db_sqlite_master <- function(con) {
  
  if (db.class(con) == "sqlite") {
    
    # Get data for each table
    df <- db_get(con, "SELECT * FROM sqlite_master")
    
  } else {
    
    stop("Support is only for SQLite databases...", call. = FALSE)
    
  }
  
  return(df)
  
}
