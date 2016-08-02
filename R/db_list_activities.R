#' Function to list activities/processes for a database. 
#' 
#' \code{db_list_activities} only supports PostgreSQL databases at present. 
#' 
#' @param con Database connection. 
#' @param json Should the return be formatted as a JSON string. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_activities <- function(con, json = FALSE) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    df <- suppressWarnings(
      db_get(con, "SELECT * FROM pg_stat_activity")
    )
    
  }
  
  # Others, at the moment
  if (grepl("sqlite|mysql", class(con), ignore.case = TRUE))
    stop("Not implemented.", call. = FALSE)
  
  # Make a json object
  if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
  
  # Return
  df
  
}

