#' Function to drop a table within a database.
#' 
#' @param con Database connection. 
#' 
#' @param table Table to drop. 
#' 
#' @param cascade Should the \code{DROP TABLE} statement also use a 
#' \code{CASCADE} clause to force the table drop? 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Invisible, \code{con}. 
#' 
#' @export
db_drop_table <- function(con, table, cascade = FALSE) {

  # Check if table exists
  if (!db_table_exists(con, table)) {
    stop("`table` does not exist in database...", call. = FALSE) 
  }
  
  # Build statement
  sql <- stringr::str_c("DROP TABLE ", table)
  
  # Add cascade to statement
  if (cascade) sql <- stringr::str_c(sql, " CASCADE")
  
  # Drop
  db_execute(con, sql)
  
  return(invisible(con))
  
}
