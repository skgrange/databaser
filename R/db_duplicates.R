#' Function to find duplicates in a database table's variable. 
#' 
#' @param con Database connection. 
#' 
#' @param table Database table. 
#' 
#' @param variable Database table's variable to test. 
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_duplicates <- function(con, table, variable) {
  
  # Build query
  sql <- stringr::str_c(
    "SELECT ", variable,
    " FROM ", table, 
    " GROUP BY ", variable, 
    " HAVING (COUNT(*) > 1)"
  )
  
  sql <- threadr::str_trim_many_spaces(sql)
  
  # Query
  df <- db_get(con, sql)
  
  return(df)
  
}


#' Function to test if duplicates are contained in a database table's variable. 
#' 
#' @param con Database connection. 
#' 
#' @param table Database table. 
#' 
#' @param variable Database table's variable to test. 
#' 
#' @return Logical vector with length of 1.  
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_has_duplicates <- function(con, table, variable) {
  
  # Get duplicates
  df <- db_duplicates(con, table, variable)
  
  # Test
  x <- ifelse(nrow(df) != 0, TRUE, FALSE)
  
  return(x)
  
}
