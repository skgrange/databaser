#' Function to build SQL \code{WHERE} statements from an input data frame. 
#' 
#' @param table Database's table to update. 
#' 
#' @param df Input data frame to generate \code{UPDATE} statements with. 
#' 
#' @param where Which variables in \code{df} to be used for the \code{WHERE} 
#' clause. If not used, only a single row data frame can be used. 
#' 
#' @param squish Should whitespace around commas and equal signs be removed to
#' make the SQL statement shorter? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with a length of \code{nrow(df)}. 
#' 
#' @export
build_update_statements <- function(table, df, where = NA, squish = FALSE) {
  
  # Check inputs
  stopifnot(length(table) == 1)
  
  # Return empty string if input is empty
  if (nrow(df) == 0) return(as.character())
  
  if (is.na(where[1]) && nrow(df) != 1) {
    stop("If `where` is not used, input must have a single row...", call. = FALSE)
  }
  
  # Build where clauses
  if (!is.na(where[1])) {
    
    # Build where clause
    sql_where <- build_sql_pairs(df[, where, drop = FALSE], sep = "and")
    sql_where <- str_c(" WHERE ", sql_where)
   
    # Drop where variable from data to be used for update statements
    df <- df[, setdiff(names(df), where), drop = FALSE]
    
  }
  
  # Create sql pairs containing the data
  sql_update <- build_sql_pairs(df, sep = ",")
  
  # Build update statement
  sql_update <- str_c("UPDATE ", table, " SET ", sql_update)
  
  # Add where clauses
  if (!is.na(where[1])) sql_update <- str_c(sql_update, sql_where)
  
  # Remove some whitespace to make statement smaller
  if (squish) {
    sql_update <- str_replace_all(sql_update, ", ", ",")
    sql_update <- str_replace_all(sql_update, " = ", "=")
  }
  
  return(sql_update)
  
}


build_sql_pairs <- function(df, sep) {
  
  # Parse
  stopifnot(length(sep) == 1)
  sep <- stringr::str_trim(sep)
  sep <- stringr::str_to_upper(sep)
  
  if (!sep %in% c(",", "AND")) {
    stop("`sep` must be one of 'AND' or ','...", call. = FALSE)
  }
  
  sep <- ifelse(sep == "AND", " AND ", sep)
  sep <- ifelse(sep == ",", ", ", sep)
  
  # Format for database table
  df <- prepare_data_frame_for_sql(df)
  
  # Get named vector for sql keys
  keys <- names(df)
  names(keys) <- keys
  
  # Use keys and values to make sql pairs
  df <- df %>% 
    purrr::map2_dfr(keys, ., ~str_c(.x, " = ", .y)) %>% 
    tidyr::unite(., "sql", 1:ncol(.), sep = sep) %>% 
    pull()
  
  return(df)
  
}


prepare_data_frame_for_sql <- function(df) {
  
  # Catch single quotes
  df <- dplyr::mutate_if(df, is.character, ~str_replace_all(., "'", "''"))
  
  # Add single quotes if character
  df <- dplyr::mutate_if(df, is.character, ~str_c("'", ., "'"))
  
  # Switch nas to sql null
  df <- dplyr::mutate_all(df, ~ifelse(is.na(.), "NULL", .))
  
  return(df)
  
}
