#' Function to get database name.
#'
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param extension For SQLite databases, should the database name include the
#' file name extension? 
#'
#' @export
db_name <- function(con, extension = TRUE) {
  
  # Get database's class
  db_class <- db_class(con)
  
  if (db_class == "postgres") {
    x <- db_get(con, "SELECT CURRENT_DATABASE()")[, 1, drop = TRUE]
  } else if (db_class %in% c("mysql", "mariadb")) {
    x <- db_get(con, "SELECT DATABASE()")[, 1, drop = TRUE]
  } else if (db_class == "sqlite") {
    # Get file name
    x <- basename(con@dbname)
    # Drop file extension, could be unreliable
    if (!extension) x <- stringr::str_split_fixed(x, "\\.", 2)[, 1]
  } else if (db_class == "sql_server") {
    x <- db_get(con, "SELECT DB_NAME() AS name")[, 1, drop = TRUE]
  } else {
    cli::cli_abort("Database connection not supported.")
  }
  
  return(x)
  
}
