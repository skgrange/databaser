#' Function to get the version of a database service.
#' 
#' Only PostgreSQL databases are supported currently. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @export
db_version <- function(con) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE))
    x <- db_get(con, "SELECT version()")[, 1]
    # SHOW server_version; SHOW server_version_num;
  
  # Return
  x
  
}
