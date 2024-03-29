% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_import_row_counts.R
\name{db_import_row_counts}
\alias{db_import_row_counts}
\alias{db_import_database_details}
\title{Functions to import somewhat standard tables from databases.}
\usage{
db_import_row_counts(con, tz = "UTC")

db_import_database_details(con, tz = "UTC")
}
\arguments{
\item{con}{Database connection.}

\item{tz}{Time zone to represent dates in.}
}
\value{
Tibble.
}
\description{
Functions to import somewhat standard tables from databases.
}
\seealso{
\code{\link{db_count_rows_insert}}, \code{\link{db_details_insert}}
}
\author{
Stuart K. Grange
}
