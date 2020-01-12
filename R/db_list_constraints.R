#' Function to list table constraints for a database. 
#' 
#' \code{db_list_constraints} only supports PostgreSQL databases at present. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
db_list_constraints <- function(con) {
  
  if (db.class(con) == "postgres") {
    df <- db_get(con, "SELECT * FROM information_schema.constraint_column_usage")
  } else {
    df <- tibble()
    warning("Database not supported...", call. = FALSE)
  }
  
  return(df)
  
}
