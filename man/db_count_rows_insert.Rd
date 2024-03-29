% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_count_rows_insert.R
\name{db_count_rows_insert}
\alias{db_count_rows_insert}
\title{Function to get row counts and insert into a database table.}
\usage{
db_count_rows_insert(
  con,
  table = "row_counts",
  estimate = FALSE,
  print = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{con}{Database connection.}

\item{table}{Database table name for row count data.}

\item{estimate}{Should the row counts be estimated rather than counted? Only
works for PostgreSQL databases.}

\item{print}{Should the current row counts be printed after the database
queries?}

\item{verbose}{Should the function give messages?}
}
\value{
Invisible \code{con}.
}
\description{
Function to get row counts and insert into a database table.
}
\seealso{
\code{\link{db_count_rows}}, \code{\link{db_details}}
}
\author{
Stuart K. Grange
}
