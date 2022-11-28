#' Function to get the size of a database.
#' 
#' For SQLite databases, the file size is returned, for PostgreSQL databases,
#' the `pg_database` table is queried, and for MySQL database 
#' `information_schema.tables` is queried.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param unit Measurement unit. Default is \code{"mb"} for megabytes. 
#' 
#' @return Numeric vector. 
#' 
#' @export
db_size <- function(con, unit = "mb") {
  
  # Parse
  unit <- stringr::str_to_lower(unit)
  
  # Get database's class
  db_class <- db.class(con)
  
  if (db_class == "sqlite") {
    
    # Just get file size
    x <- threadr::file_size(con@dbname, unit = "none")
    
  } else if (db_class == "postgres") {
    
    # Build query, requires database's name
    sql <- glue::glue("SELECT pg_database_size('{db_name(con)}') AS size")
    
    # Query database
    x <- db_get(con, sql) %>% 
      pull() %>% 
      as.numeric()
    
  } else if (db_class %in% c("mysql", "mariadb")) {
    
    # Build query, also requires database's name
    sql <- glue::glue(
      "SELECT table_schema AS name,
      SUM(data_length + index_length) AS size
      FROM information_schema.tables 
      WHERE table_schema = '{db_name(con)}'
      GROUP BY table_schema"
    )
    
    # Query database
    x <- pull(db_get(con, sql), size)
    
  } else {
    stop("Not implemented", call. = FALSE)
  }
  
  # Convert unit
  if (unit == "mb") x <- x / 1e+06
  if (unit == "gb") x <- x / 1e+09
  
  # Return
  return(x)
  
}
