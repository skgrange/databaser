#' Function to determine database connection class/type. 
#' 
#' \code{db.class} will return a lower-case string of database type which is 
#' suitable for logic testing similar to \code{\link{is.numeric}} or 
#' \code{\link{is.character}}. 
#' 
#' @author Stuart K. Grange.
#' 
#' @param con Database connection. 
#' 
#' @return Character vector with a length of 1.
#' 
#' @export
db.class <- function(con) {
  
  # Get class
  x <- stringr::str_to_lower(class(con)[1])
  
  # Switch to lower case
  x <- dplyr::case_when(
    stringr::str_detect(x, "(?i)sqlite") ~ "sqlite",
    stringr::str_detect(x, "(?i)mysql") ~ "mysql",
    stringr::str_detect(x, "(?i)maria") ~ "mariadb",
    stringr::str_detect(x, "(?i)postgres|pqconnection") ~ "postgres",
    stringr::str_detect(x, "(?i)SQL Server") ~ "sql_server"
  )
  
  # Stop if database service is unknown
  if (is.na(x)) {
    stop("Unknown database service.", call. = FALSE)
  }
  
  return(x)
    
}


# Create an alias too
#' @rdname db.class
#' @export
db_class <- db.class
