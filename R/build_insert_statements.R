#' Function to build \code{INSERT INTO} SQL statements from a data frame. 
#' 
#' @param table Database table name to insert into. 
#' 
#' @param df Data frame to use to create statements. 
#' 
#' @param squish Should whitespace around commas and equal signs be removed to
#' make the SQL statement shorter? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with a length of nrow(df). 
#' 
#' @export
build_insert_statements <- function(table, df, squish = FALSE) {
  
  # Check inputs
  stopifnot(length(table) == 1)
  
  # No quoting used for names
  insert_into <- str_c(names(df), collapse = ", ")
  insert_into <- str_c("INSERT INTO ", table, " (", insert_into, ")")
  
  # Prepare data frame
  df <- prepare_data_frame_for_sql(df)
  
  # Collapse rows and create values piece of sql string
  sql <- tidyr::unite(df, "values", 1:ncol(df), sep = ", ") %>% 
    mutate(values = str_c(" VALUES (", values, ")")) %>% 
    pull() %>% 
    str_c(insert_into, .)
  
  # Remove some whitespace to make statement smaller
  if (squish) sql <- str_replace_all(sql, ", ", ",")
  
  return(sql)
  
}
