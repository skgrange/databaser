#' Function to read a SQL script and store statments as a vector. 
#' 
#' To-do: Mutli-line commenting is not correctly parsed yet, fix. 
#' 
#' @seealso \code{\link{db_send}}, \code{\link{db_get}}
#' 
#' @author Stuart K. Grange
#' 
#' @param file File name of SQL script. 
#' 
#' @return Character vector. 
#' 
#' @export
read_sql <- function(file) {
  
  # Load file
  sql <- readLines(file, warn = FALSE, encoding = "UTF-8")
  
  # Drop comments
  sql <- grep("--", sql, invert = TRUE, value = TRUE)
  
  # Clean
  sql <- stringr::str_c(sql, collapse = "")
  sql <- stringr::str_squish(sql)
  
  # Split based on ;
  sql <- stringr::str_split(sql, ";")[[1]]
  
  # Drop empty statements
  sql <- sql[sql != ""]
  
  return(sql)
  
}
