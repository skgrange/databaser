#' Function to copy a database table from one connection to another. 
#' 
#' @param con_source Database connection to the source database. 
#' 
#' @param con_destination Database connection to the destination database. 
#' 
#' @param table Table to copy from \code{con_source} to \code{con_destination}. 
#' 
#' @param exists_only Should only the tables which exist in both databases be
#' copied? 
#' 
#' @param verbose Should the function give messages?  
#' 
#' @return Invisible \code{con_source}. 
#' 
#' @export
db_copy_table_between_connections <- function(con_source, con_destination, 
                                              table, exists_only = TRUE, 
                                              verbose = FALSE) {
  
  if (exists_only) {
    
    # Check if the tables exist in both databases  
    tables_exist <- all(
      c(db_table_exists(con_source, table), db_table_exists(con_destination, table))
    )
    
    # If not, stop
    if (!tables_exist) {
      cli::cli_abort(
        "`{table}` does not exist in the source and destination databases."
      )
    }
  }
  
  # Load table from source database
  if (verbose) {
    cli::cli_alert_info("{threadr::cli_date()} Reading `{table}` from source...")
  }
  
  df <- databaser::db_get(
    con_source,
    stringr::str_glue(
      "SELECT *
      FROM {table}"
    )
  )
  
  # Insert into destination database
  if (verbose) {
    cli::cli_alert_info(
      "{threadr::cli_date()} Inserting `{table}` into destination..."
    )
  }
  
  # Insert
  databaser::db_insert(con_destination, table, df, replace = FALSE)
  
  return(invisible(con_source))
  
}
