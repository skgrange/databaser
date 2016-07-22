#' Function to build SQL UPDATE statements. 
#' 
#' @author Stuart K. Grange
#' 
#' @importFrom stringr str_c
#' 
#' @export
sql_update_builder <- function(sql, table) {
  
  # Apply function
  sql <- plyr::llply(sql, sql_update_builder_worker, table = table)
  
  # To character vector
  sql <- unlist(sql)
  
  # Drop attributes
  attributes(sql) <- NULL
  
  # Return
  sql
  
}


sql_update_builder_worker <- function(sql, table) {
  
  # Get where, to-do will need to be more flexable
  sql_where <- sql[1]
  
  # Collapse the vector
  sql <- str_c(sql, collapse = ",")
  
  # The statment builder
  sql <- str_c("UPDATE ", table, " SET ", sql, " WHERE ", sql_where)
  
  # Return
  sql
  
}
