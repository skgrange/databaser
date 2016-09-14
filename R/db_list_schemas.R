#' Function to list schemas for a database. 
#' 
#' \code{db_list_schemas} only supports PostgreSQL databases at present. 
#' 
#' @param con Database connection. 
#' @param json Should the return be formatted as a JSON string. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
db_list_schemas <- function(con, json = FALSE) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    # Query
    df <- db_get(con, "SELECT catalog_name, 
                       schema_name, 
                       schema_owner
                       FROM information_schema.schemata")
    
  } else {
    
    stop("Not implemented.", call. = FALSE)
    
  }
  
  # Make a json object
  if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
    
  # Return
  df
  
}
