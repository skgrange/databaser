#' Function to list data types of database tables. 
#' 
#' \code{db_list_data_types} does not support MySQL databases at present. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Tables to get data types for. If not used, all tables will be
#' returned. 
#' 
#' @return Data frame. 
#' 
#' @export
db_list_data_types <- function(con, table = NA) {
  
  # Postgres
  if (db.class(con) == "postgres") {
    
    if (is.na(table[1])) {
      
      # Make sql
      sql <- stringr::str_c(
        "SELECT table_name, 
         column_name, 
         data_type 
         FROM information_schema.columns"
      )
      
    } else {
      
      # For sql
      table <- threadr::str_sql_quote(table)
      
      # Make sql
      sql <- stringr::str_c(
       "SELECT table_name, 
        column_name, 
        data_type 
        FROM information_schema.columns 
        WHERE table_name IN (", table, ")"
      )
      
    }
    
    # Clean statement
    sql <- threadr::str_trim_many_spaces(sql)
    
    # Query
    df <- db_get(con, sql)
    
  } else if (db.class(con) == "sqlite") {
    
    if (is.na(table[1])) {
      # Get table vector
      table <- db_list_tables(con)
    } 
    
    # Do for all tables
    df <- table %>% 
      purrr::map(~db_list_data_types_sql_lite_worker(con, .)) %>% 
      purrr::list_rbind()
    
  } else {
    stop("Not implemented.", call. = FALSE)
  }
  
  return(df)
  
}


db_list_data_types_sql_lite_worker <- function(con, table) {
  
  # Build sql
  sql <- stringr::str_c("PRAGMA table_info(", table, ")")
  
  # Query database
  df <- db_get(con, sql)
  
  # Add variable 
  df$table_name <- table
  
  # Select and rename
  df <- dplyr::select(df, table_name, variable = name, data_type = type)
  
  return(df)
  
}
