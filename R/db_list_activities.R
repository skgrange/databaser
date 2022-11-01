#' Function to list activities/processes for a database. 
#' 
#' \code{db_list_activities} supports PostgreSQL and MySQL databases at present. 
#' 
#' @param con Database connection. 
#' 
#' @param json Should the return be formatted as a JSON string? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_activities <- function(con, json = FALSE) {
  
  if (db.class(con) == "postgres") {
    df <- suppressWarnings(db_get(con, "SELECT * FROM pg_stat_activity"))
  } else if (db.class(con) %in% c("mysql", "mariadb")) {
    df <- db_get(con, "SHOW FULL PROCESSLIST")
  } else if (db.class(con) == "sqlite") {
    stop("Not implemented.", call. = FALSE)
  } else {
    stop("Database not suported.", call. = FALSE)
  }
  
  # Make a json object, not a useful name here
  if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
  
  return(df)
  
}
