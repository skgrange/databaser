#' Function to prepare an R data frame for SQL statement usage. 
#' 
#' \code{sql_formatter} will quote strings, factors, and dates and will also 
#' convert \code{NA}s to \code{NULL}s.
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame. 
#' 
#' @importFrom stringr str_replace_all str_c
#' 
#' @export
sql_formatter <- function(df) {
  
  # No factors
  df <- threadr::factor_coerce(df)
  
  # Quote strings and dates
  index_character <- sapply(df, function(x) 
    ifelse(is.character(x) | is.Date(x) | lubridate::is.POSIXct(x), TRUE, FALSE))
  
  # Escape single quotes
  df[index_character] <- lapply(df[index_character], 
                                function(x) str_replace_all(x, "'", "''"))
  
  # Quote characters
  df[index_character] <- lapply(df[index_character], 
                                function(x) str_c("'", x, "'"))
  
  # NA to NULL, no quoting here
  df[] <- lapply(df, function(x) ifelse(is.na(x), "NULL", x))
  
  # Return
  df
  
}
