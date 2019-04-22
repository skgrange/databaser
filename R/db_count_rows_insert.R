#' Function to get row counts and insert into a database table. 
#' 
#' @param con Database connection. 
#' 
#' @param table Database table name for row count data.
#' 
#' @param estimate Should the row counts be estimated rather than counted? Only
#' works for PostgreSQL databases.
#' 
#' @param print Should the current row counts be printed after the database
#' queries? 
#' 
#' @seealso \code{\link{db_count_rows}}, \code{\link{db_details}}
#' 
#' @author Stuart K. Grange.
#' 
#' @return Invisible \code{con}. 
#' 
#' @export
db_count_rows_insert <- function(con, table = "row_counts", estimate = FALSE, 
                                 print = FALSE) {
  
  # Get start date
  date_system <- as.numeric(lubridate::now())
  
  # Get all tables
  tables_db <- databaser::db_list_tables(con)
   
  # # But do not do `row_counts` table
  # tables_db <- setdiff(tables_db, table)
  
  # Get counts of all tables
  df <- databaser::db_count_rows(con, table = tables_db, estimate = estimate) %>% 
    mutate(system = threadr::hostname(),
           date = date_system) %>% 
    select(system,
           date, 
           everything())
  
  # Print current row counts
  if (print) {
    df %>% 
      arrange(-row_count) %>% 
      print(n = Inf)
  }
  
  # Insert into database
  databaser::db_insert(con, table, df, replace = FALSE)

  return(invisible(con))
  
}
