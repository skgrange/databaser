#' Function to quickly return tables, variables, and values in a database. 
#' 
#' \code{db_contents} will return a tidy data frame with three variables, 
#' \code{"table"}, \code{"variable"}, and \code{"value"}. \code{db_contents} is 
#' useful to explore a database's structure. 
#' 
#' If there are complexities around schemas, PostGIS tables, temporary tables, 
#' or permissions, the table name is returned with a single variable of \code{NA}.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param limit Maximum number rows to return in each table. Default is 1. Use 
#' \code{NA} to read entire table; use with caution. 
#' 
#' @examples 
#' \dontrun{
#' # Only one value will be returned
#' data_single_value <- db_contents(con)
#' 
#' # Entire tables will be returned
#' data_entire_contents <- db_contents(con, limit = NA)
#' }
#'
#' @export
db_contents <- function(con, limit = 1, process = "text") {
  
  .Deprecated()
  
  # Get tables
  tables <- db_list_tables(con)
  
  # Apply function
  df <- plyr::ldply(tables, db_contents_worker, con, limit = limit, 
                    .progress = process)
  
  # Return
  df
  
}


# The function which does the work
# 
# No export
db_contents_worker <- function(table, con, limit = NA) {
  
  if (is.na(limit)) {
    
    # Read entire table
    df <- tryCatch({
      
      db_read_table(con, table)
      
    }, error = function(e) {
      
      data.frame(table = character())
      
    })
    
  } else {
    
    # Only read n rows/observations
    df <- tryCatch({
      
      df <- db_get(con, stringr::str_c("SELECT * FROM ", table, " LIMIT ", limit))
      
      # Postgres sometimes returns null if table does not exist, so raise error
      if (is.null(df)) stop()
      
      # Return
      df
      
    }, error = function(e) {
      
      data.frame(table = character())
      
    })
    
  }
  
  # A catch if the table cannot be accessed, e.g. PostGIS extensions
  if (nrow(df) == 0) {
    
    df <- data.frame(
      table = table, 
      variable = NA, 
      value = NA,
      stringsAsFactors = FALSE
    )
    
  } else {
    
    # Add table variable
    df$table <- table
    
    # This will keep the table-variable in position 1
    suppressWarnings(
      df <- tidyr::gather(df, variable, value, -table)
    )
    
  }
  
  # Return
  df
  
}
