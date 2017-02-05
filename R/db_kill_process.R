#' Function to kill a database process. 
#' 
#' Currently, only PostgreSQL databases are supported.  
#'
#' @author Stuart K. Grange
#' 
#' @param con Database connection. 
#' 
#' @param process Process ID to kill. Use \code{\link{db_list_activities}} to 
#' find this integer. 
#' 
#' @export
db_kill_process <- function(con, process) {
  
  # Postgres
  if (grepl("postgres", class(con), ignore.case = TRUE)) {
    
    # Build
    sql <- stringr::str_c("SELECT pg_cancel_backend(", process, ")")
    
    # Use
    db_execute(con, sql)
    
  }
  
  # No return
  
}
