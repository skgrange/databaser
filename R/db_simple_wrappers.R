#' Function to read an entire database table. 
#' 
#' \code{db_read_table} is a wrapper for \code{DBI::dbReadTable}. 
#' 
#' @param con Database connection. 
#' 
#' @param table Table to read. 
#' 
#' @param verbose Should the function print messages?
#' 
#' @return Tibble. 
#' 
#' @export
db_read_table <- function(con, table, verbose = FALSE) {
  if (verbose) message(threadr::date_message(), "Reading `", table, "`...")
  dplyr::as_tibble(DBI::dbReadTable(con, table))
}


#' Function to list all variables/columns/fields in a database table. 
#' 
#' \code{db_list_variables} is a wrapper for \code{DBI::dbListFields}. 
#' 
#' @param con Database connection. 
#' 
#' @param table Table to list variables/columns/fields. 
#' 
#' @return Character vector. 
#' 
#' @export
db_list_variables <- function(con, table) DBI::dbListFields(con, table)


#' Function to list results for a database connection. 
#' 
#' @param con Database connection. 
#' 
#' @export
db_list_results <- function(con) DBI::dbListResults(con)[[1]]


#' Function to clear results for a database connection. 
#' 
#' @param con Database connection.
#' 
#' @export
db_clear_results <- function(con) DBI::dbClearResult(db_list_results(con))


#' Function to commit transactions for a database connection. 
#' 
#' @param con Database connection.
#' 
#' @export
db_commit <- function(con) DBI::dbCommit(con)


#' Function to list database tables.
#' 
#' @param con Database connection.
#' 
#' @return Character vector. 
#' 
#' @export
db_list_tables <- function(con) DBI::dbListTables(con)


#' Function to test if database contains a table.
#' 
#' @param con Database connection.
#' 
#' @param table A table name. 
#' 
#' @return Logical vector. 
#' 
#' @export
db_table_exists <- function(con, table) {
  table %>% 
    purrr::set_names(.) %>% 
    purrr::map_lgl(~DBI::dbExistsTable(con, .))
}


#' Function open a database transaction, perform operations, and commit the 
#' results if a series of successful operations are performed. 
#' 
#' @param con Database connection.
#' 
#' @param code R code that interacts with \code{con}. 
#' 
#' @param ... Other parameters passed on to methods.
#' 
#' @return Invisible logical vector. 
#'
#' @seealso \code{\link{dbWithTransaction}}
#' 
#' @export
db_with_transaction <- function(con, code, ...) {
  DBI::dbWithTransaction(conn = con, code = code, ...) %>% 
    invisible()
}
