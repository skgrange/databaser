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
  
  if (db_class(con) == "postgres") {
    df <- db_get(con, "SELECT * FROM INFORMATION_SCHEMA.COLUMNS")
  } else {
    cli::cli_abort("Not implemented.")
  }
  
  return(df)
  
}
