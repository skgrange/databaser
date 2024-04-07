#' Function to get a database's table permissions information for users and
#' tables. 
#' 
#' Currently, \code{db_table_permissions} only supports Postgres databases. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con A database connection.
#' 
#' @param user A vector of database users. 
#' 
#' @param table A vector of database tables. 
#' 
#' @return Tibble. 
#' 
#' @export
db_table_permissions <- function(con, user = NA, table = NA) {
  
  if (db_class(con) == "postgres") {
    
    # Use current user as default
    if (is.na(user[1])) {
      user <- db_get(con, "SELECT CURRENT_USER") %>% 
        pull()
    }
    
    # Use all tables as a default
    if (is.na(table[1])) {
      table <- db_list_tables(con)
    }
    
    # Build select statement
    sql_select <- stringr::str_glue(
      "SELECT *
      FROM information_schema.role_table_grants 
      WHERE grantee IN ({str_sql_collapse(user)})
      AND table_name IN ({str_sql_collapse(table)})
      ORDER BY grantee,
      table_name,
      privilege_type"
    )
    
  } else {
    cli::cli_abort("Database type not supported.")
  }
  
  # Use sql statement
  df <- db_get(con, sql_select)
  
  return(df)
  
}
