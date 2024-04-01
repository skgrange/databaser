#' Function to list activities/processes for a database. 
#' 
#' \code{db_list_activities} supports PostgreSQL and MySQL databases at present. 
#' 
#' @param con Database connection. 
#' 
#' @param only_con Filter activities only to the database where \code{con} has 
#' been made. 
#' 
#' @param as_json Should the return be formatted as a JSON string? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_activities <- function(con, only_con = FALSE, as_json = FALSE) {
  
  # Get activities/processes
  if (db_class(con) == "postgres") {
    df <- db_get(con, "SELECT * FROM pg_stat_activity")
  } else if (db_class(con) %in% c("mysql", "mariadb")) {
    df <- db_get(con, "SHOW FULL PROCESSLIST")
  } else if (db_class(con) == "sqlite") {
    cli::cli_abort("Not implemented.")
  } else {
    cli::cli_abort("Database not suported.")
  }
  
  # Filter to the connected database, only one backend used here at the moment
  if (only_con) {
    if (db_class(con) == "postgres") {
      df <- filter(df, datname == db_name(con))
    } 
  }
  
  # Make a json object, not a useful name here and catch some additional classes
  if (as_json) {
    df <- df %>% 
      mutate(
        across(tidyselect::vars_select_helpers$where(is_pq_inet), as.character),
        across(tidyselect::vars_select_helpers$where(is_pq_xid), as.character)
      ) %>% 
    jsonlite::toJSON(pretty = TRUE)
  }
  
  
  
  return(df)
  
}


is_pq_inet <- function(x) {
  inherits(x, "pq_inet")
}


is_pq_xid <- function(x) {
  inherits(x, "pq_xid")
}
