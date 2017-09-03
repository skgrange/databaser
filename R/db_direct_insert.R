#' Function to connect and insert directly into a database. 
#' 
#' \code{db_direct_insert} has been designed so a database can be connected to,
#' data inserted, and then disconnected with a single function call. 
#' 
#' @param file \code{JSON} file or string containing database connection 
#' details. For SQLite databases, use the database's file path. See 
#' \code{\link{db_connect}} for more information. 
#' 
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument 
#' is not needed and will be ignored if used. 
#' 
#' @param table Table in, or to be created in \code{con}.
#' 
#' @param df Data frame to be inserted into \code{con}.
#' 
#' @param replace Should the database table be replaced? Default is \code{FALSE}. 
#' Be cautious using this argument because it will drop the database table if it
#' exists. 
#' 
#' @return Invisible.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{db_connect}}, \code{\link{db_insert}}, 
#' \code{\link{db_direct_read}}
#' 
#' @export
db_direct_insert <- function(file, database = NA, table, df, replace = FALSE) {
  
  # Connect to database
  con <- db_connect(file, database)
  
  # Insert df into table
  db_insert(con, table = table, df = df, replace = replace)
  
  # Disconnect from database
  db_disconnect(con)
  
  # No return
  
}


#' Function to connect and read database table directly. 
#' 
#' \code{db_direct_read} has been designed so a database can be connected to,
#' data read, data returned, and then disconnected with a single function call. 
#' 
#' @param file \code{JSON} file or string containing database connection 
#' details. For SQLite databases, use the database's file path. See 
#' \code{\link{db_connect}} for more information. 
#' 
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument 
#' is not needed and will be ignored if used. 
#' 
#' @param table Table in, or to be created in \code{con}.
#' 
#' @return Data frame.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{db_connect}}, \code{\link{db_insert}}, 
#' \code{\link{db_direct_insert}}
#' 
#' @export
db_direct_read <- function(file, database, table) {
  
  # Connect to database
  con <- db_connect(file, database)
  
  # Insert df into table
  df <- db_get(con, stringr::str_c("SELECT * FROM ", table))
  
  # Disconnect from database
  db_disconnect(con)
  
  return(df)
  
}
