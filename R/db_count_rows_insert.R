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
#' @param verbose Should the function give messages?
#' 
#' @seealso \code{\link{db_count_rows}}, \code{\link{db_details}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \code{con}. 
#' 
#' @export
db_count_rows_insert <- function(con, table = "row_counts", estimate = FALSE, 
                                 print = FALSE, verbose = FALSE) {
  
  # Get start date
  date_system <- as.numeric(lubridate::now())
  
  # Get all tables
  tables_db <- db_list_tables(con)
  
  # Get counts of all tables
  df <- db_count_rows(
    con, 
    table = tables_db, 
    estimate = estimate,
    verbose = verbose
  ) %>% 
    mutate(system = threadr::hostname(),
           date = date_system) %>% 
    relocate(system,
             date)
  
  # Print current row counts
  if (print) {
    df %>% 
      arrange(-row_count) %>% 
      print(n = Inf)
  }
  
  # Insert into database
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Inserting row counts data...")
  }
  
  db_insert(con, table, df, replace = FALSE)

  return(invisible(con))
  
}
