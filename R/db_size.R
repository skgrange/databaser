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
#' @param as_fs_bytes Should the return be of the \code{fs_bytes} data type? If
#' this argument is used, \code{unit} is not used. 
#' 
#' @return Numeric or \code{fs_bytes} vector with a length of \code{1}. 
#' 
#' @export
db_size <- function(con, unit = "mb", as_fs_bytes = FALSE) {
  
  # Parse input
  unit <- stringr::str_to_lower(unit)
  
  # Get database's class
  db_class <- db_class(con)
  
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
    
  } else if (db_class == "sql_server") {
    
    # A non-standard function and requires filtering to the database name, fs 
    # function will pass the units correctly
    x <- db_get(con, "exec sp_spaceused") %>% 
      filter(database_name == !!db_name(con)) %>% 
      pull(database_size) %>% 
      fs::as_fs_bytes() %>% 
      as.numeric()
    
  } else {
    cli::cli_abort("Database not supported.")
  }
  
  # Format return
  if (as_fs_bytes) {
    # Change data type
    x <- fs::as_fs_bytes(x)
  } else {
    # Convert units
    x <- dplyr::case_when(
      unit == "mb" ~ x / 1e6,
      unit == "gb" ~ x / 1e9,
      .default = x
    )
  }
  
  return(x)
  
}
