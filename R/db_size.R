#' Function to get the size of a database.
#' 
#' For SQLite databases, the file size is returned but for PostgreSQL databases,
#' the `pg_database` table is queried.  
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param unit Measurement unit for SQLite databases. Default is \code{"mb"} for
#' megabytes. 
#' 
#' @return Numeric vector. 
#' 
#' @export
db_size <- function(con, unit = "mb") {
  
  # Parse
  unit <- stringr::str_to_lower(unit)
  
  if (db.class(con) == "sqlite") {
    
    x <- threadr::file_size(con@dbname, unit = "none")
    
    # Convert unit
    if (unit == "mb") x <- x / 1e+06
    if (unit == "gb") x <- x / 1e+09
    
  } else if (db.class(con) == "postgres") {
    
    # x <- db_get(con, "SELECT SUM(pg_database_size(oid)) FROM pg_database")[, 1]
    
    # Build query, requires name
    sql_select <- str_c(
      "SELECT pg_size_pretty(pg_database_size('", 
      db_name(con), 
      "'))"
    )
    
    # Query
    x <- db_get(con, sql_select)[, ]
    
  } else {
    
    stop("Not implemented...", call. = FALSE)
    
  }
  
  # Return
  return(x)
  
}
