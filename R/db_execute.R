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
#' @return Invisible.
#' 
#' @export
db_execute <- function(con, statement, ..., progress = "none") {
  
  # # Set progress bar off if verbose
  # if (verbose) progress <- "none"
  
  # Do
  plyr::l_ply(
    statement, 
    function(x) DBI::dbExecute(con, x, ...),
    .progress = progress
  )
  
  # No return
  
}


# db_execute_worker <- function(con, x, verbose, ...) {
#   
#   if (verbose) message(x)
#   DBI::dbExecute(con, x, ...)
#   
# }
