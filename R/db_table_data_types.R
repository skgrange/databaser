#' Function to get a database tables' data types.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' 
#' @param table An optional character vector of table names to filter the return
#' to. 
#' 
#' @return Tibble.
#' 
#' @export
db_table_data_types <- function(con, table = NA) {
  
  if (db_class(con) == "postgres") {
    
    # Build the select statement
    sql_select <- stringr::str_glue(
      "SELECT table_catalog,
      table_schema,
      table_name,
      ordinal_position,
      column_name,
      data_type,
      udt_name,
      is_nullable,
      numeric_precision,
      character_maximum_length
      FROM information_schema.columns
      ORDER BY table_name,
      ordinal_position"
    )
    
    # Introduce where clause if table has been passed
    if (!is.na(table[1])) {
      
      # Collapse table vector for sql
      table_collapsed <- table %>% 
        stringr::str_c("'", ., "'") %>% 
        stringr::str_c(collapse = ",")
      
      # Build where clause
      sql_where <- stringr::str_glue(
        "WHERE table_name IN ({table_collapsed}) \nORDER BY"
      )
      
      # Introduce where clause into select statement
      sql_select <- stringr::str_replace(sql_select, "ORDER BY", sql_where)
      
    }
    
    # Query database
    df <- db_get(con, sql_select)
    
  } else {
    cli::cli_abort("Database type not supported.")
  }
  
  return(df)
  
}
