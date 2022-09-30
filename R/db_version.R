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
  
  if (db.class(con) == "sqlite") {
    x <- db_get(con, "SELECT sqlite_version()")[, 1]
  } else if (db.class(con) %in% c("mysql", "maria")) {
    stop("Not implemented.", call. = FALSE)
  } else if (db.class(con) == "postgres") {
    x <- db_get(con, "SELECT version()")[, 1, drop = TRUE]
  } else {
    stop("Database connection not supported.", call. = FALSE)
  }
    
  return(x)
  
}
