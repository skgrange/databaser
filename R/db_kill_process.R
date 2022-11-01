#' Function to kill a database process. 
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
  
  if (db.class(con) == "postgres") {
    sql <- stringr::str_c("SELECT pg_cancel_backend(", process, ")")
    db_execute(con, sql)
  } else if (db.class(con) %in% c("mysql", "mariadb")) {
    sql <- stringr::str_c("kill ", process)
    db_execute(con, sql)
  }
  
  return(invisible(con))
  
}
