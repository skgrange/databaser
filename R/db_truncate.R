#' Function to run a \code{TRUNCATE TABLE} statement for a database table. 
#' 
#' Beware that this function will remove the contents of a database table so use
#' with caution. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' @param table Table to remove all data from. 
#' 
#' @return Invisible. 
#' 
#' @export
db_version <- function(con, table) {
  
  if (db.class(con) == "sqlite") 
    sql <- stringr::str_c("DELETE FROM ", table)
  
  if (db.class(con) %in% c("mysql", "postgres")) 
    sql <- stringr::str_c("TRUNCATE TABLE ", table)
  
  # Do
  db_execute(con, sql)
  
  # No return
  
}
