#' Function to list users for a database. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get the database's users
#' db_list_users(con)
#' 
#' }
#' 
#' @export
db_list_users <- function(con) {
  
  if (db_class(con) == "postgres") {
    df <- db_get(con, "SELECT * FROM pg_user")
  } else if (db_class(con) %in% c("mysql", "mariadb")) {
    df <- db_get(con, "SELECT User AS user FROM mysql.user")
  } else {
    cli::cli_abort("Not implemented.")
  }
  
  return(df)
  
}
