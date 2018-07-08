#' Function to build SQL \code{WHERE} statements from an input data frame. 
#' 
#' @param table Database's table to update. 
#' 
#' @param df Input data frame to generate \code{UPDATE} statements with. 
#' 
#' @param where Which variable in \code{df} to be used for the \code{WHERE} 
#' clause. If not used, only a single row data frame can be used. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#' 
#' @export
build_update_statements <- function(table, df, where = NA) {
  
  # Check inputs
  stopifnot(length(table) == 1)
  stopifnot(length(where) == 1)
  
  if (is.na(where) && nrow(df) != 1)
    stop("If `where` is not used, input must have a single row...", call. = FALSE)
  
  if (!is.na(where)) {
    
    # Get where variable's values
    values_where <- df[, where, drop = TRUE]
    
    # Quote if needed
    if (is.character(values_where)) 
      values_where <- str_c("'", values_where, "'")
    
    # Drop where variable from data to be used for update statements
    df <- df[, setdiff(names(df), where)]
    
  }
  
  # Add quotes if character
  df <- dplyr::mutate_if(df, is.character, dplyr::funs(str_c("'", ., "'")))
  
  # Switch nas to sql null
  df <- dplyr::mutate_all(df, dplyr::funs(ifelse(is.na(.), "NULL", .)))
  
  # Get named vector for sql keys
  keys <- names(df)
  names(keys) <- keys
  
  # Use keys and values to make sql pairs
  df <- purrr::map2_dfr(keys, df, ~str_c(.x, " = ", .y))
  
  # Unite all pairs together
  df <- tidyr::unite(df, "united", 1:ncol(df), sep = ", ")
  
  # Build update statements
  x <- str_c("UPDATE ", table, " SET ", pull(df))
  
  # Add where clauses
  if (!is.na(where)) x <- str_c(x, " WHERE ", where, " = ", values_where)
  
  return(x)
  
}
