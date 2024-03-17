#' Function to return some high level details about a database. 
#' 
#' @param con Database connection. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return Tibble with one observation. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Get some information about the database
#' db_details(con)
#' 
#' }
#' 
#' @export
db_details <- function(con) {
  
  # Get table vector, this is used multiple times
  tables <- db_list_tables(con)
  
  # Build tibble
  tibble(
    system = threadr::hostname(),
    date = as.numeric(lubridate::now()),
    class = db.class(con),
    size = db_size(con, unit = "mb"),
    table_count = length(tables),
    table_names = stringr::str_c(tables, collapse = "; ")
  )
  
}


#' Function to return some high level details about a database. 
#' 
#' @param con Database connection. 
#' 
#' @param table Database table to insert details into. 
#' 
#' @param print Should \code{table} be printed post-insert? 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return Invisible \code{con}. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Get some information about the database and insert into table
#' db_details_insert(con)
#' 
#' }
#' 
#' @export
db_details_insert <- function(con, table = "database_details", print = FALSE) {
  
  # Get details
  df <- db_details(con)
  
  # Insert into database
  db_insert(con, table, df, replace = FALSE)
  
  # Read and print table after insert
  if (print) {
    con %>% 
      db_get(
        stringr::str_glue(
          "SELECT * 
          FROM {table}
          ORDER BY date"
        )
      ) %>% 
      print(n = Inf)
  }
  
  return(invisible(con))
  
}
