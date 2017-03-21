#' Function to list activities/processes for a database. 
#' 
#' \code{db_list_activities} supports PostgreSQL and MySQL databases at present. 
#' 
#' @param con Database connection. 
#' @param json Should the return be formatted as a JSON string? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_activities <- function(con, json = FALSE) {
  
  if (grepl("postgres", class(con)[1], ignore.case = TRUE)) {
    
    df <- suppressWarnings(
      db_get(con, "SELECT * FROM pg_stat_activity")
    )
    
  }
  
  if (grepl("mysql", class(con), ignore.case = TRUE))
    df <- db_get(con, "SHOW FULL PROCESSLIST")
  
  # Others
  if (grepl("sqlite", class(con), ignore.case = TRUE))
    stop("Not implemented.", call. = FALSE)
  
  # Make a json object
  if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
  
  # Return
  df
  
}
