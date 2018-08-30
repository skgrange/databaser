#' Function to create a database connection with a \code{JSON} configuration file. 
#' 
#' \code{db_connect} uses a \code{JSON} configuration file to create a database
#' connection. This configuration file will often exist outside a code package 
#' so database credentials are not accidentally transmitted or shared. 
#' 
#' If only one entry is in the \code{JSON} file, the \code{database} argument is
#' not needed.
#' 
#' If \code{db_connect} is attempted to be used with a file which is not 
#' \code{JSON}, it will attempt to connect to an SQLite database, but it is 
#' recommended that the \code{config} argument is set to \code{FALSE} in this 
#' case. 
#'
#' MySQL, PostgreSQL, and SQLite connections are currently supported. 
#' 
#' @param file \code{JSON} file or string containing database connection 
#' details. For SQLite databases, use the database's file path. 
#' 
#' @param database The name of the database within \code{file} to use to create
#' a database connection to. If only one entry is in \code{file}, this argument 
#' is not needed and will be ignored if used. 
#' 
#' @param config A logical to skip the \code{JSON} file configuration and just
#' attempt to connect to \code{file} directly. This is used for SQLite
#' databases which require no configuration. 
#' 
#' @param foreign_keys A logical for SQLite databases where foreign keys should 
#' be enforced. Default is \code{FALSE}. For other database types, this will be 
#' ignored. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Database connection. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Connect to an air quality database
#' db <- db_connect("connections.json", "air_quality")
#' 
#' 
#' # Use a json config file which looks similar to this:
#' string <- '
#' {
#'   "driver": "MySQL",
#'   "host": "172.31.4.159",
#'   "database_name": "database_seven",
#'   "user": "web",
#'   "password": "read_password"
#' }'
#' 
#' # Connect, no need for second argument when one connection is present in 
#' # configuration file
#' db_seven <- db_connect(string)
#' 
#' 
#' # A SQLite connection needs no configuration
#' con <- db_connect("../databases/air_quality_data.db", config = FALSE)
#' 
#' }
#' 
#' @export
db_connect <- function(file, database, config = TRUE, foreign_keys = FALSE) {
  
  # Load configuration file
  json <- tryCatch({
    
    # Read
    jsonlite::fromJSON(file)
    
  }, error = function(e) {
    
    # Capture and return error text
    conditionMessage(e)
    
  })
  
  # If config is TRUE and the file is not json, attempt SQLite connection
  if (any(grepl("lexical error|sqlite|parse error", json, ignore.case = TRUE))) {
    
    # Set config to false for SQLite databases
    config <- FALSE
    
  }
  
  if (config) {
    
    # If json file has many database connection details, filter with argument
    if (class(json) == "data.frame")
      json <- json[json[, "database_name"] == database, ]
    
    # Create connection based on driver type
    if (grepl("mysql", json$driver, ignore.case = TRUE)) {
      
      # Connect
      con <- DBI::dbConnect(
        RMySQL::MySQL(), 
        host = json$host, 
        dbname = json$database_name,
        user = json$user, 
        password = json$password
      )
      
    } else if (grepl("postg", json$driver, ignore.case = TRUE)) {
      
      # Connect
      con <- DBI::dbConnect(
        RPostgreSQL::PostgreSQL(), 
        host = json$host, 
        dbname = json$database_name,
        user = json$user, 
        password = json$password
      )
      
      # Also give application name
      db_execute(con, stringr::str_c(
        "SET application_name = '", 
        postgres_application_name(), "'")
      )
      
    }
    
  } else {
    
    # sqlite databases, only need a path
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    
    # Add support for foreign keys
    if (foreign_keys) db_execute(con, "PRAGMA foreign_keys = 1")
    
  }
  
  return(con)
  
}


#' Function to close a database connection cleanly.  
#' 
#' @param con An active database connection.
#' 
#' @return Invisible.  
#' 
#' @export
db_disconnect <- function(con) quiet(DBI::dbDisconnect(con))


postgres_application_name <- function() {
  
  # Get infomation
  list_version <- R.Version()
  
  # Clean
  version <- list_version$version.string
  version <- stringr::str_remove_all(version, "\\s*\\([^\\)]+\\)")
  version <- stringr::str_remove(version, "version ")
  
  # Add package name too
  version <- stringr::str_c(version, " with RPostgreSQL")
  
  return(version)
  
}
