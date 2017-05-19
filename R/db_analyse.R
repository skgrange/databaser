#' Function to analyse/analyze a database table. 
#' 
#' @param con Database connection. 
#' @param table Database table. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible.
#' 
#' @export
db_analyse <- function(con, table) {
  
  # Postgres
  if (db.class(con) == "postgres")
    db_send(con, stringr::str_c("ANALYZE ", table))
  
  # No return
  
}
