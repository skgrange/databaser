#' Function to get SQLite table information (\code{PRAGMA table_info}). 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' 
#' @param table Table name. 
#' 
#' @return Data frame. 
#' 
#' @export
db_table_info <- function(con, table = NA) {
  
  if (db.class(con) == "sqlite") {
    
    if (is.na(table[1])) table <- db_list_tables(con)
    
    # Build queries
    sql_select <- stringr::str_c("PRAGMA table_info(", table, ")")
    names(sql_select) <- table
    
    # Get data for each table
    df <- purrr::map_dfr(sql_select, ~db_get(con, .x), .id = "table")
    
  } else {
    
    stop("Support is only for SQLite databases...", call. = FALSE)
    
  }
  
  return(df)
  
}
