#' Function to drop a database table. 
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
#' @return Invisible \code{con}. 
#' 
#' @export
db_drop_table <- function(con, table, cascade = FALSE) {
  
  # One one table can be dropped at a time
  if (length(table) != 1L) {
    cli::cli_abort("`table` must have a length of 1.")
  }
  
  # Check if table exists
  stopifnot(db_table_exists(con, table))
  
  # Build statement
  sql <- stringr::str_c("DROP TABLE ", table)
  
  # Add cascade to statement
  if (cascade) {
    sql <- stringr::str_c(sql, " CASCADE")
  }
  
  # Do, drop the table
  db_execute(con, sql)
  
  return(invisible(con))
  
}
