#' Function to collapse vectors into a vector with a length of one for use in
#' SQL statements. 
#' 
#' \code{str_sql_collapse} will quote character and factor vectors before being
#' collapsed. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x A vector to be collapsed. 
#' 
#' @return A character vector with a length of one. 
#' 
#' @examples
#' 
#' # Collapse an integer vector
#' str_sql_collapse(1:10)
#' 
#' # Collapse a character vector
#' str_sql_collapse(letters[1:10])
#' 
#' @export
str_sql_collapse <- function(x) {
  
  if (is.character(x) || is.factor(x)) {
    x <- stringr::str_c("'", x, "'")
  }
  
  x <- stringr::str_c(x, collapse = ",")
  
  return(x)
  
}
