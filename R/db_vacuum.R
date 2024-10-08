#' Functions to vacuum, analyse, and optimise database tables. 
#' 
#' The PostgreSQL statement uses \code{VACUUM (VERBOSE)}, the MySQL statement
#' uses \code{OPTIMIZE}, and the SQLite statement uses \code{VACUUM} and the 
#' \code{table} argument is not used. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Database table. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible tibble. 
#' 
#' @examples 
#' \dontrun{
#' 
#' db_vacuum(con, "model_data")
#' db_optimise(con, "air_quality_data")
#' 
#' }
#' 
#' @export
db_vacuum <- function(con, table = NA, verbose = FALSE) {
  
  # Message to user
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Vacuuming database...")
  }
  
  # Used for logic
  db_class <- db_class(con)
  
  # Get things pre-vacuum
  date_pre <- Sys.time()
  size_pre <- db_size(con)
  
  if (db_class == "postgres") {
    
    # Default to all tables
    if (is.na(table[1])) {
      table <- db_list_tables(con)
    }
    
    # Do
    db_execute(con, stringr::str_c("VACUUM (VERBOSE) ", table))
    
  } else if (db_class %in% c("mysql", "mariadb")) {
    
    # Catch the reserved verbs
    table <- stringr::str_c("`", table, "`")
    
    # Optimise
    db_execute(con, stringr::str_c("OPTIMIZE TABLE ", table))
    
  } else if (db_class == "sql_server") {
    
    # Default to all tables
    if (is.na(table[1])) {
      table <- db_list_tables(con)
    }
    
    # A rough equivalent to vacuum
    db_execute(con, stringr::str_glue("UPDATE STATISTICS {table}"))
    
  } else if (db_class == "sqlite") {
    db_execute(con, "VACUUM")
  }
  
  # Get things post-vacuum
  date_post <- Sys.time()
  size_post <- db_size(con)
  
  # Build tibble return
  df <- tibble(
    when = c("pre_vacuum", "post_vacuum"),
    date = c(date_pre, date_post),
    size = c(size_pre, size_post)
  )
  
  return(invisible(df)) 
  
}


# Use MySQL notation
#' @rdname db_vacuum
#' @export
db_optimise <- db_vacuum
