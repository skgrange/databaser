#' Functions to vacuum, analyse, and optimise database tables. 
#' 
#' The PostgreSQL statement uses \code{VACUUM (VERBOSE)}, the MySQL statement
#' uses \code{OPTIMIZE}, and the SQLite statement uses \code{VACUUM} and the 
#' \code{table} argument is not used. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param table Database table. 
#' 
#' @param return Should the function return a data frame containing the database
#' sizes pre- and post-vacuum? 
#' 
#' @return Invisible or data frame if \code{return = TRUE}. 
#' 
#' @examples 
#' \dontrun{
#' 
#' db_vacuum_analyse(con, "model_data")
#' db_optimise(con, "air_quality_data")
#' 
#' }
#' 
#' @export
db_vacuum <- function(con, table, return = FALSE) {
  
  # Get things pre-vacuum
  if (return) {
    
    date_pre <- Sys.time()
    size_pre <- db_size(con)
    
  }
  
  # PostgreSQL databases
  if (grepl("postgresql", class(con), ignore.case = TRUE))
    quiet(db_execute(con, stringr::str_c("VACUUM (VERBOSE) ", table)))
  
  # MySQL databases
  if (grepl("mysql", class(con), ignore.case = TRUE)) {
    
    # Catch the reserved verbs
    table <- stringr::str_c("`", table, "`")
    
    # Optimise
    quiet(db_execute(con, stringr::str_c("OPTIMIZE TABLE ", table)))
    
  }
  
  # SQLite
  if (grepl("sqlite", class(con), ignore.case = TRUE))
    quiet(db_execute(con, "VACUUM"))
  
  # Get things post-vacuum
  if (return) {
    
    date_post <- Sys.time()
    size_post <- db_size(con)
    
    df <- data.frame(
      when = c("pre_vacuum", "post_vacuum"),
      date = c(date_pre, date_post), 
      size = c(size_pre, size_post), 
      stringsAsFactors = FALSE
    )
    
    return(df)
    
  } else {
   
    return(invisible()) 
    
  }
  
}


# Use MySQL notation
#' @rdname db_vacuum
#' @export
db_optimise <- db_vacuum
