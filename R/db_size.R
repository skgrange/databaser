#' Function to get the size of a database.
#' 
#' For SQLite databases, the file size is returned but for PostgreSQL databases,
#' the `pg_database` table is queried.  
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @export
db_size <- function(con, unit = "mb") {
  
  # SQlite
  if (grepl("sqlite", class(con)[1], ignore.case = TRUE)) {
    
    file_name <- con@dbname
    x <- threadr::file_size(file_name, unit = unit)
    
  }
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    x <- db_get(con, "SELECT SUM(pg_database_size(oid)) FROM pg_database")[, 1]
    
    if (unit == "mb") x <- x / 1e+06
    if (unit == "gb") x <- x / 1e+09
    
  }
  
  # Return
  x
  
}
