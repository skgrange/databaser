#' Function to determine database connection class/type. 
#' 
#' \code{db.class} will return a lower-case string of database type which is 
#' suitable for logic testing similar to \code{\link{is.numeric}} or 
#' \code{\link{is.character}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @return Character vector with a length of one. 
#' 
#' @export
db.class <- function(con) {
  
  # Get class
  class <- stringr::str_to_lower(class(con)[1])
  
  # Switch, TODO, use case_when
  if (grepl("sqlite", class)) class_string <- "sqlite"
  if (grepl("mysql", class)) class_string <- "mysql"
  if (grepl("maria", class)) class_string <- "maria"
  if (grepl("postgres", class)) class_string <- "postgres"
  
  if (!exists("class_string")) stop("Unknown database service.", call. = FALSE)
  
  # Return
  class_string
    
}
