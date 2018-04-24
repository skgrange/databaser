#' Function to return all variable names in all tables in a SQL database. 
#' 
#' If there are complexities around schemas, PostGIS tables, or temporary tables, 
#' the table name is returned with a single variable of \code{NA}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Get variables
#' table_variables <- db_variables(con)
#' 
#' }
#'
#' @export
db_variables <- function(con) {
  
  # Get all table's names
  df <- purrr::map_dfr(db_list_tables(con), ~db_variables_worker(con, .x)) %>% 
    arrange(table)
  
  return(df)
  
}


# Function to get table and variable names from database table
# 
# No export
db_variables_worker <- function(con, table) {
  
  # Get vector of variables from database table
  variables <- tryCatch({
    
    db_list_variables(con, table) 
    
  }, error = function(e) {
    
    NA
    
  })
  
  # Make data frame
  df <- data.frame(
    table = table, 
    variable = variables,
    stringsAsFactors = FALSE
  )
  
  return(df)
  
}
