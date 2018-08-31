#' Function to read an entire database table. 
#' 
#' \code{db_read_table} is a wrapper for \code{DBI::dbReadTable}. 
#' 
#' @param con Database connection. 
#' 
#' @param table Table to read. 
#' 
#' @return Tibble. 
#' 
#' @export
db_read_table <- function(con, table) 
  dplyr::as_tibble(DBI::dbReadTable(con, table))


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
#' @param table A table name. 
#' 
#' @return Logical vector. 
#' 
#' @export
db_table_exists <- function(con, table) DBI::dbExistsTable(con, table)
