#' Function to get the names of database table and produce a data frame with 
#' zero rows. 
#' 
#' \code{db_table_names} is useful when preparing a data frame for a SQL insert.
#' 
#' @param con Database connection.
#' 
#' @param table Database table.
#' 
#' @param as_tibble Should the return be a tibble? 
#'
#' @author Stuart K. Grange
#' 
#' @return Data frame with zero rows. 
#'
#' @export
db_table_names <- function(con, table, as_tibble = FALSE) {
  
  # Get database table names with one observation
  df <- db_get(
    con, 
    stringr::str_c("SELECT * FROM ", table, " LIMIT 1"), 
    warn = FALSE
  )
  
  # Postgres returns 0 0 data frame
  if (nrow(df) == 0 & ncol(df) == 0) {
    
    # Get names
    names <- db_list_variables(con, table)
    
    # Create a data frame
    df <- read.csv(text = "", col.names = names)
    
  }
  
  # Remove data if present
  df <- df[-1, ]
  
  # To tibble
  if (as_tibble) df <- as_tibble()
  
  return(df)
  
}
