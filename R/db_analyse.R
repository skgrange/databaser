#' Function to analyse/analyze a database table. 
#' 
#' @param con Database connection. 
#' @param table Database table. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_analyse <- function(con, table) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE))
    db_send(con, stringr::str_c("ANALYZE ", table))
  
  # No return
  
}
