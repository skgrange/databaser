#' Function to execute statements on a database. 
#' 
#' \code{db_execute} is a wrapper for \code{DBI::dbExecute} but is vectorised 
#' over \code{statement}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param statement Statement to send to \code{con}. 
#' 
#' @param ... Other parameters passed on to methods.  
#' 
#' @param progress Type of progress bar to display. 
#' 
#' @return Invisible \code{con}. 
#' 
#' @export
db_execute <- function(con, statement, ..., progress = FALSE) {
  
  # A switch for old input
  if (progress == "time") {
    progress <- TRUE
  }
  
  # Do
  purrr::walk(
    statement, 
    function(x) DBI::dbExecute(con, x, ...),
    .progress = progress
  )
  
  return(invisible(con))
  
}
