#' Function to list users for a database. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get users
#' db_list_users(con)
#' 
#' }
#' 
#' @export
db_list_users <- function(con) {
  
  if (grepl("postgres", class(con)[1], ignore.case = TRUE)) {
    
    df <- suppressWarnings(
      db_get(con, "SELECT * FROM pg_user")
    )
    
  } 
  
  if (grepl("mysql", class(con), ignore.case = TRUE))
    df <- db_get(con, "SELECT User AS user FROM mysql.user")
  
  if (grepl("sqlite", class(con), ignore.case = TRUE))
    stop("Not implemented.", call. = FALSE)
  
  return(df)
  
}
