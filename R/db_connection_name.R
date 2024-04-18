#' Functions to get a database connection strings/names that are suitable for 
#' printing.
#' 
#' @author Stuart K. Grange
#' 
#' @param con A database connection. 
#' 
#' @return A character or \strong{glue} character vector with length of \code{1}.
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
  x <- as.character(stringr::str_glue("<{connection}> {connection_details$host}"))
  
  return(x)
  
}


#' @rdname db_connection_name
#' @export
cli_db_connection <- function(con) {
  cli::cli_alert_info(
    "{threadr::cli_date()} Connected to: `{db_connection_name(con)}`..."
  )
}
