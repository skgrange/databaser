#' Function to test for wildcards and then raise an error if found. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Argument to test. 
#' 
#' @param type Type of wildcards to detect. Only SQL is implemented.
#' 
#' @return Invisible \code{x}.
#' 
#' @export
db_wildcard_check <- function(x, type = "sql") {
  
  # Check for sql wildcard
  if (type == "sql") {
    
    if (grepl("%|\\*", str_c(x, collapse = " "))) 
      stop("Inputs cannot contain SQL wildcards...", call. = FALSE)
    
  }
  
  return(invisible(x))
  
}
