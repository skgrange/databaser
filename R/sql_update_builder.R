#' Function to build SQL UPDATE statements. 
#' 
#' \code{sql_update_builder} will use the first key-value pair to build the 
#' WHERE clause. To-do: make better.  
#' 
#' @author Stuart K. Grange
#' 
#' @importFrom stringr str_c
#' 
#' @return Character vector.  
#' 
#' @export
sql_update_builder <- function(sql, table) {
  
  # Apply function
  sql <- plyr::llply(sql, sql_update_builder_worker, table = table)
  
  # To character vector
  sql <- unlist(sql)
  
  # Drop attributes
  attributes(sql) <- NULL
  
  return(sql)
  
}


sql_update_builder_worker <- function(sql, table) {
  
  # Get where, to-do will need to be more flexible
  sql_where <- sql[1]
  
  # Collapse the vector
  sql <- str_c(sql, collapse = ",")
  
  # The statment builder
  sql <- str_c("UPDATE ", table, " SET ", sql, " WHERE ", sql_where)
  
  return(sql)
  
}
