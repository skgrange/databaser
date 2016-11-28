#' Function to get database name.
#'
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param extension For SQLite databases, should the database name include the
#' file name extension? 
#'
#' @export
db_name <- function(con, extension = TRUE) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE))
    x <- db_get(con, "SELECT CURRENT_DATABASE()")[, 1]
  
  # MySQL
  if (grepl("mysql", class(con), ignore.case = TRUE))
    x <- db_get(con, "SELECT DATABASE()")[, 1]
  
  # SQLite
  if (grepl("sqlite", class(con), ignore.case = TRUE)) {
    
    # Get file name
    x <- basename(con@dbname)
    
    # Drop file extension, could be unreliable
    if (!extension) x <- stringr::str_split_fixed(x, "\\.", 2)[, 1]
    
  }
  
  # Return
  x 
  
}
