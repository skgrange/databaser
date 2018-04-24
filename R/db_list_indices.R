#' Function to list indices for a database tables. 
#' 
#' @param con Database connection. 
#' 
#' @param table A vector of table names. If unused, all tables will be queried.  
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_indices <- function(con) {

  if (db.class(con) == "sqlite") {
    
    df <- purrr::map_dfr(db_list_tables(con), ~db_list_indices_worker(con, .x))
    
  } else if (db.class(con) == "postgres") {
    
    df <- db_get(con, "SELECT * FROM pg_indexes")
    
  } else {
    
    stop("Database not supported...", call. = FALSE)
    
  }
    
  return(df)
  
}


# Ony for SQLite
db_list_indices_worker <- function(con, table) {
  
  # Build statement
  sql <- stringr::str_c("PRAGMA INDEX_LIST('", table, "')")
  
  # Query
  df <- db_get(con, sql)
  
  return(df)
  
}
