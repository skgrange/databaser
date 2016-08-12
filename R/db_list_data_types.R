#' Function to list data types of database tables. 
#' 
#' \code{db_list_data_types} only supports PostgreSQL databases at present. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' @param table Tables to get data types for. If not used, all tables will be
#' returned. 
#' @param Should the return be in pretty JSON? Default is \code{FALSE}. 
#' 
#' @export
db_list_data_types <- function(con, table = NA, json = FALSE) {
  
  # Postgres only at the moment
  if (grepl("postgres", class(con), ignore.case = TRUE)) { 
    
    if (is.na(table)) {
      
      # Make sql
      sql <- stringr::str_c("SELECT table_name, column_name, 
                          data_type 
                          FROM information_schema.columns")
      
    } else {
      
      # For sql
      table <- threadr::str_sql_quote(table)
      
      # Make sql
      sql <- stringr::str_c("SELECT table_name, column_name, 
                          data_type 
                          FROM information_schema.columns 
                          WHERE table_name IN (", table, ")")
      
    }
    
    # Clean statement
    sql <- threadr::str_trim_many_spaces(sql)
    
    # Query
    df <- db_get(con, sql)
    
    # To json
    if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
    
  } else {
    
    stop("Not implemented.", call. = FALSE)
    
  }
  
  # Return
  df
  
}
