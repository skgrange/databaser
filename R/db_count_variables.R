#' Function to count variables/columns/fields in a database table. 
#' 
#' @param con Database connection. 
#' 
#' @param table Table to get variables for. If \code{NA}, all tables will be 
#' queried. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_count_variables <- function(con, table = NA) {
  
  if (is.na(table[1])) table <- db_list_tables(con)
  df <- purrr::map_dfr(table, ~db_count_variables_worker(con, table = .x))
  return(df)
  
}


db_count_variables_worker <- function(con, table) {
  
  data.frame(
    table = table,
    variable_count = length(db_list_variables(con, table)),
    stringsAsFactors = FALSE
  )
  
}
