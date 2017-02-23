#' Function to list users for a database. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame
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
  
  if (grepl("postgre", class(con)[1], ignore.case = TRUE)){
    
    df <- suppressWarnings(
      db_get(con, "SELECT * FROM pg_user")
    )
    
  } else {
    
    stop("Not implemented...", call. = FALSE)
    
  }
  
  # Return
  df
  
}
