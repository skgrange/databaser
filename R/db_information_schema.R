#' Function to get schema information from a database. 
#' 
#' Only PostgreSQL databases are supported currently. 
#'
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @export
db_information_schema <- function(con) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    df <- db_get(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
    
  } else {
    
    stop("Database type not supported.", call. = FALSE)
    
  }
  
  # Return
  df
  
}
