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
#' @param progress Type of progress bar to display. Default is \code{"none"}. 
#' 
#' @return Invisible.
#' 
#' @export
db_execute <- function(con, statement, ..., progress = "none") {
  
  # Do
  plyr::l_ply(
    statement, 
    function(x) DBI::dbExecute(con, x, ...),
    .progress = progress
  )
  
  # No return
  
}
