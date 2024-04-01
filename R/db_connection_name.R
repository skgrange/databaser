#' Function to get a database connection string/name that is suitable for 
#' printing.
#' 
#' @author Stuart K. Grange
#' 
#' @param con A database connection. 
#' 
#' @return A \strong{glue} character vector with length of \code{1}.
#' 
#' @export
db_connection_name <- function(con) {
  
  # Get a few vectors
  connection <- as.character(class(con)[1])
  connection_details <- DBI::dbGetInfo(con)
  
  # If no host for sqlite databases
  if (is.na(connection_details$host)) {
    connection_details$host <- "localhost"
  }
  
  # Build a string that is good for printing
  x <- stringr::str_glue(
    "{connection}; {connection_details$host}; {connection_details$dbname}"
  )
  
  return(x)
  
}
