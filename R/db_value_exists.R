#' Function to test if a value exists in a database table's column/variable. 
#' 
#' @param con Database connection. 
#' 
#' @param table Table in database. 
#' 
#' @param variable Variable/column in \code{table}. 
#' 
#' @param value Value to test in \code{table} and \code{variable}. 
#' 
#' @return Logical vector with length of 1. 
#' 
#' @author Stuart K. Grange
#'
#' @export 
db_value_exists <- function(con, table, variable, value) {
  
  sql <- stringr::str_c(
    "SELECT COUNT(*) AS count 
    FROM ", table,
    " WHERE ", variable, " = '", 
    value, "'"
  )
  
  x <- db_get(con, sql)
  x <- ifelse(x[, 1] != 0, TRUE, FALSE)
  x
  
}
