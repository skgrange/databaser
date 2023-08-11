#' Function to arrange variables in a data frame/tibble to match those found in 
#' a database table. 
#' 
#' \code{db_arrange_variables} is useful for preparing data for insert.
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Table name in database.
#'  
#' @param df Data frame/tibble.
#' 
#' @return \code{df}, possibly with rearranged and/or additional variables.
#' 
#' @export
db_arrange_variables <- function(con, table, df) {
  bind_rows(db_table_names(con, "table"), df)
}
