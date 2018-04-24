#' Function to test for wildcards and then raise an error if found. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Argument to test. 
#' 
#' @param type Type of wildcards to detect. Only SQL is implemented.
#' 
#' @return Invisible, called for the side effect of an error.  
#' 
db_wildcard_check <- function(x, type = "sql") {
  
  # Collapse
  x <- str_c(x, collapse = " ")
  
  # Check for sql wildcard
  if (type == "sql") {
    
    if (grepl("%|\\*|\\?", x)) 
      stop("Inputs cannot contain SQL wildcards...", call. = FALSE)
    
  }
  
  # No return
  
}
