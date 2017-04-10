#' Function to get the version of a database service.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @return Character vector
#' 
#' @export
db_version <- function(con) {
  
  # SQLite
  if (db.class(con) == "sqlite") 
    x <- db_get(con, "SELECT sqlite_version()")[, 1]
  
  if (db.class(con) == "mysql") 
    stop("Not implemented...", call. = FALSE)
  
  # Postgres
  if (db.class(con) == "postgres")
    x <- db_get(con, "SELECT version()")[, 1]
  
  # Return
  x
  
}
