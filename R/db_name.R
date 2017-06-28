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
  
  if (db.class(con) == "postgres")
    x <- db_get(con, "SELECT CURRENT_DATABASE()")[, 1]
  
  if (db.class(con) == "mysql")
    x <- db_get(con, "SELECT DATABASE()")[, 1]
  
  if (db.class(con) == "sqlite") {
    
    # Get file name
    x <- basename(con@dbname)
    
    # Drop file extension, could be unreliable
    if (!extension) x <- stringr::str_split_fixed(x, "\\.", 2)[, 1]
    
  }
  
  # Return
  x 
  
}
