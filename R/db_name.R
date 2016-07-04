#' Function to get database name.
#'
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#'
#' @export
db_name <- function(con) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE))
    x <- db_get(con, "SELECT CURRENT_DATABASE()")[, 1]
  
  # MySQL
  if (grepl("mysql", class(con), ignore.case = TRUE))
    x <- db_get(con, "SELECT DATABASE()")[, 1]
  
  # REturn
  x 
  
}
