#' Function to insert a data frame into/as a database table (R data frame to 
#' SQL table). 
#'
#' \code{db_insert} is a wrapper for \code{\link{dbWriteTable}}, but uses 
#' different defaults. \code{db_insert} will not replace data by default. 
#' 
#' @seealso \code{\link{dbWriteTable}}, \code{\link{db_list_variables}},
#' \code{\link{db_table_names}}, \code{\link{db_arrange_variables}}
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection.
#'
#' @param table Table in, or to be created in \code{con}.
#'
#' @param df Data frame to be inserted into \code{con}.
#'
#' @param replace Should the database table be replaced? Default is \code{FALSE}. 
#' Be cautious using this argument because it will drop the database table if it
#' exists. 
#'
#' @export
db_insert <- function(con, table, df, replace = FALSE) {
  
  # Switch replace
  if (replace) {
    
    append <- FALSE
    overwrite <- TRUE
    
  }
  
  # Catch dplyr's data table
  df <- threadr::base_df(df)
  
  # Write data frame to database
  # Do not display cat output
  quiet(
    DBI::dbWriteTable(con, table, df, append = append, overwrite = overwrite, 
                      row.names = FALSE)
  )
  
  # No return
  
}
