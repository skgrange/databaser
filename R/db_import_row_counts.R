#' Functions to import somewhat standard tables from databases. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param tz Time zone to represent dates in.
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{db_count_rows_insert}}, \code{\link{db_details_insert}}
#' 
#' @export
db_import_row_counts <- function(con, tz = "UTC") {
  
  stopifnot(db_table_exists(con, "row_counts"))
  
  db_get(
    con, 
    "SELECT * 
    FROM row_counts
    ORDER BY date,
    'table'"
  ) %>% 
    mutate(date = threadr::parse_unix_time(date, tz = tz))
  
}


#' @rdname db_import_row_counts
#' @export
db_import_database_details <- function(con, tz = "UTC") {
  
  stopifnot(db_table_exists(con, "database_details"))
  
  db_get(
    con, 
    "SELECT * 
    FROM database_details
    ORDER BY date"
  ) %>% 
    mutate(date = threadr::parse_unix_time(date, tz = tz))
  
}
