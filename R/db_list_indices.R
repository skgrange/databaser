#' Function to list indices for a database tables. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_indices <- function(con) {
  
  if (db_class(con) == "sqlite") {
    df <- db_list_tables(con) %>% 
      purrr::map(~db_list_indices_worker(con, .x)) %>% 
      purrr::keep(~nrow(.) != 0L) %>% 
      purrr::list_rbind()
  } else if (db_class(con) == "postgres") {
    df <- db_get(con, "SELECT * FROM pg_indexes")
  } else if (db_class(con) %in% c("mysql", "mariadb")) {
    df <- db_get(con, "SELECT * FROM information_schema.statistics")
  } else {
    cli::cli_abort("Database not supported.")
  }
    
  return(df)
  
}


# For SQLite
db_list_indices_worker <- function(con, table) {

  db_get(
    con, 
    stringr::str_glue(
      "PRAGMA INDEX_LIST('{table}')"
    )
  )
  
}
