#' Function to create key-value strings for SQL statements. 
#' 
#' @param df Data frame which has been formated for SQL. 
#' 
#' @return List containing SQL key-value vectors. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{sql_formatter}}
#' 
#' @importFrom stringr str_c
#'
#' @export
sql_key_value <- function(df) {
  
  # Key names only once
  keys <- names(df)
  
  # Row-wise application of function
  sql <- plyr::alply(df, 1, sql_key_value_worker, key = keys)
  
  # Drop attributes
  attributes(sql) <- NULL
  
  # Return
  sql
  
}


sql_key_value_worker <- function(x, key) str_c(key, "=", x)
