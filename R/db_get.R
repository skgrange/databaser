#' Function to get/fetch data from a database with a statement. 
#' 
#' \code{db_get} is a wrapper for \code{DBI::dbGetQuery}. 
#' 
#' @param con Database connection. 
#' 
#' @param statement Statement to send to \code{con}. 
#' 
#' @param warn Should the function return warnings? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
db_get <- function(con, statement, warn = TRUE) {
  
  # Get data
  if (warn) {
    df <- DBI::dbGetQuery(con, statement)
  } else {
    df <- suppressWarnings(
      DBI::dbGetQuery(con, statement)
    )
  }
  
  # Always a tibble
  df <- dplyr::as_tibble(df)
  
  return(df)
  
}
