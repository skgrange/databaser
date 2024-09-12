#' Function to get row counts from database tables. 
#' 
#' The \code{row_count} variable will be a numeric value rather than an integer 
#' to avoid integer overflow issues for large tables. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' 
#' @param table Table name in \code{con}. If \code{table} is \code{NA}, all tables
#' in \code{con} will be queried. 
#' 
#' @param estimate Only for PostgreSQL, should the table count estimate be used?
#' 
#' @param verbose Should the function give messages?
#' 
#' @return Tibble.
#' 
#' @export
db_count_rows <- function(con, table = NA, estimate = FALSE, verbose = FALSE) {
  
  # If no table is selected, do them all
  if (is.na(table[1])) {
    table <- db_list_tables(con)
  }
  
  # Check database
  if (length(table) == 0) {
    cli::cli_abort("The database has no tables.")
  }
  
  # Do
  df <- purrr::map(
    table, 
    ~db_count_rows_worker(
      con, 
      table = .x, 
      estimate = estimate, 
      verbose = verbose
    )
  ) %>% 
    purrr::list_rbind()
  
  return(df)
  
}


# Function to get the row counts
db_count_rows_worker <- function(con, table, estimate, verbose) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Counting rows in `{table}`...")
  }
  
  if (estimate) {
    
    # Will only work for postgres
    sql <- str_c(
      "SELECT reltuples::bigint AS row_count 
      FROM pg_class 
      WHERE relname = '", table, "'"
    )

  } else {
    
    # Create statement, use real so 32 bit integers are not a limitation
    sql <- str_c(
      "SELECT CAST(COUNT(*) AS REAL) AS row_count 
      FROM ", 
      table
    )
    
    # Do not use the cast function here
    if (db.class(con) %in% c("mysql", "mariadb", "sql_server")) {
      sql <- str_c(
        "SELECT COUNT(*) AS row_count 
         FROM ", table
      )
    }
      
  }
  
  # Use statement
  df <- tryCatch({
    db_get(con, stringr::str_squish(sql))
  }, error = function(e) {
    tibble(row_count = NA)
  })

  # Add table and order variables
  df <- tibble(table, row_count = df$row_count)
  
  return(df)
  
}
