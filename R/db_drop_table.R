#' Function to drop a table within a database. 
#' 
#' @param con Database connection. 
#' @param table Table to drop. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Invisible. 
#' 
#' @export
db_drop_table <- function(con, table) {

  # Check if table exists
  if (!db_table_exists(con, table)) 
    stop("`table` does not exist in database...", call. = FALSE)
  
  # Build statement
  sql <- stringr::str_c("DROP TABLE ", table)
  
  # Do
  db_execute(con, sql)
  
  # No return
  
}
