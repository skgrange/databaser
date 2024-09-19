#' Function to run a \code{TRUNCATE TABLE} statement for a database table. 
#' 
#' Beware that this function will remove the contents of a database table, and
#' possibly other tables so use with caution. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Table to remove all data from. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @param cascade Should the \code{TRUNCATE} statement be used with 
#' \code{CASCADE}? Using \code{cascade} will delete relations which are 
#' dependent on \code{table}. 
#' 
#' @return Invisible \code{con}.
#' 
#' @export
db_truncate_table <- function(con, table, cascade = FALSE, verbose = FALSE, 
                              progress = FALSE) {
  
  # Get database class
  db_class <- db_class(con)
  
  # Truncate n tables
  purrr::walk(
    table, 
    ~db_truncate_table_worker(
      con, db_class = db_class, table = ., cascade = cascade, verbose = verbose
    ),
    .progress = progress
  )
  
}


db_truncate_table_worker <- function(con, db_class, table, cascade, verbose) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Truncating the `{table}` table...")
  }
  
  # A switch for different database types
  if (db_class == "sqlite") {
    sql <- stringr::str_c("DELETE FROM ", table)
  } else if (db_class %in% c("mysql", "postgres", "sql_server")) {
    sql <- stringr::str_c("TRUNCATE TABLE ", table)
    if (cascade) {
      sql <- stringr::str_c(sql, " CASCADE")
    }
  } else {
    cli::cli_abort("Database not supported.")
  }
  
  # Truncate table
  db_execute(con, sql)
  
  return(invisible(con))
  
}
