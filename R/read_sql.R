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
  sql <- readLines(file, warn = FALSE)
  
  # Drop comments
  sql <- grep("--", sql, invert = TRUE, value = TRUE)
  
  # Clean
  sql <- stringr::str_c(sql, collapse = "")
  sql <- threadr::str_trim_many_spaces(sql)
  
  # Split based on ;
  sql <- unlist(stringr::str_split(sql, ";"))
  
  # Drop empty statements
  sql <- sql[!ifelse(sql == "", TRUE, FALSE)]
  
  # Return
  sql
  
}
