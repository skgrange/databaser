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
#' be enforced. Default is \code{TRUE}. For other database types, this will be 
#' ignored. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Database connection. 
#' 
#' @examples
#' 
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
db_connect <- function(file, database, config = TRUE, foreign_keys = TRUE) {
  
  # Could use mime type
  # mime::guess_type(file)
  
  # Load configuration file
  json <- tryCatch({
    jsonlite::fromJSON(file)
  }, error = function(e) {
    # Capture and return error text
    conditionMessage(e)
  })
  
  # If config is TRUE and the file is not json, attempt SQLite connection
  if (any(grepl("lexical error|sqlite|parse error", json, ignore.case = TRUE))) {
    config <- FALSE
  }
  
  if (config) {
    # If json file has many database connection details, filter with argument
    if (inherits(json, "data.frame")) {
      json <- json[json[, "database_name"] == database, ]
    }
    
    # Create connection based on driver type
    if (stringr::str_detect(json$driver, "(?i)mysql|mariadb")) {
      
      # Add a port if it does not exist
      if (!"port" %in% names(json)) {
        json$port <- 0L
      }
      
      # Connect
      con <- DBI::dbConnect(
        RMariaDB::MariaDB(), 
        host = json$host, 
        port = json$port,
        dbname = json$database_name,
        user = json$user, 
        password = json$password,
        bigint = json$bigint
      )
      
    } else if (stringr::str_detect(json$driver, "(?i)postg")) {
      
      # Add a port if it does not exist
      if (!"port" %in% names(json)) {
        json$port <- NULL
      }
      
      # Add a bigint default it does not exist
      if (!"bigint" %in% names(json)) {
        json$bigint <- "integer64"
      }
      
      if (!"sslmode" %in% names(json)) {
        json$sslmode <- "prefer"
      }
      
      # Connect
      con <- DBI::dbConnect(
        RPostgres::Postgres(), 
        host = json$host, 
        port = json$port,
        dbname = json$database_name,
        user = json$user, 
        password = json$password,
        sslmode = json$sslmode,
        bigint = json$bigint
      )
      
      # Also set the application name
      db_execute(
        con, 
        stringr::str_glue(
          "SET application_name = '{postgres_application_name()}'"
        )
      )
      
    } else if (stringr::str_detect(json$driver, "(?i)sql server")) {
      
      # Add a port if it does not exist
      if (!"port" %in% names(json)) {
        json$port <- NULL
      }
      
      # Drop Microsoft from driver name if required
      json$driver <- json$driver %>% 
        stringr::str_remove("(?i)microsoft") %>% 
        stringr::str_squish()
      
      # Make connection to MS SQL Server database
      con <- DBI::dbConnect(
        odbc::odbc(),
        driver = json$driver,
        server = json$host,
        database = json$database_name,
        uid = json$user,
        pwd = json$password,
        port = json$port,
        bigint = json$bigint
      )
      
    }
    
  } else {
    
    # Sqlite databases, only need a path
    con <- DBI::dbConnect(RSQLite::SQLite(), file)
    
    # Add support for foreign keys
    if (foreign_keys) {
      db_execute(con, "PRAGMA foreign_keys = 1")
    }
    
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
  
  # Get information
  list_version <- R.Version()
  
  # Clean
  version <- list_version$version.string
  version <- stringr::str_remove_all(version, "\\s*\\([^\\)]+\\)")
  version <- stringr::str_remove(version, "version ")
  
  # Add package name too
  version <- stringr::str_c(version, " with RPostgres")
  
  return(version)
  
}
