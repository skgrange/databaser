#' Function to get first \emph{n} rows of a database table. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#' 
#' @param table Database table name. 
#' 
#' @param n Number of rows to read. Default is 5. 
#' 
#' @return Tibble
#' 
#' @export
db_head <- function(con, table, n = 5) {
  
  if (db_class(con) == "sql_server") {
    # SQL Server uses the TOP function 
    sql <- stringr::str_glue(
      "SELECT TOP {n} * 
      FROM {table}"
    )
  } else {
    sql <- stringr::str_glue(
      "SELECT * 
      FROM {table} 
      LIMIT {n}"
    )
  }

  # Query database  
  db_get(con, sql)
  
}
