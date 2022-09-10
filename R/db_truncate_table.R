#' Function to run a \code{TRUNCATE TABLE} statement for a database table. 
#' 
#' Beware that this function will remove the contents of a database table, and
#' possibly other tables so use with caution. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Table to remove all data from. 
#' 
#' @param cascade Should the \code{TRUNCATE} statement be used with 
#' \code{CASCADE}? Using \code{cascade} will delete relations which are 
#' dependent on \code{table}. 
#' 
#' @return Invisible. 
#' 
#' @export
db_truncate_table <- function(con, table, cascade = FALSE) {
  purrr::walk(table, ~db_truncate_table_worker(con, table = ., cascade = cascade))
}


db_truncate_table_worker <- function(con, table, cascade) {
  
  # A switch for different database types
  if (db.class(con) == "sqlite") {
    sql <- stringr::str_c("DELETE FROM ", table)
  } else if (db.class(con) %in% c("mysql", "postgres")) {
    sql <- stringr::str_c("TRUNCATE TABLE ", table)
    if (cascade) sql <- stringr::str_c(sql, " CASCADE")
  }
  
  # Do
  db_execute(con, sql)
  
  return(invisible(con))
  
}
