% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_drop_table.R
\name{db_drop_table}
\alias{db_drop_table}
\title{Function to drop a database table.}
\usage{
db_drop_table(con, table, cascade = FALSE)
}
\arguments{
\item{con}{Database connection.}

\item{table}{Table to drop.}

\item{cascade}{Should the \code{DROP TABLE} statement also use a 
\code{CASCADE} clause to force the table drop?}
}
\value{
Invisible \code{con}.
}
\description{
Function to drop a database table.
}
\author{
Stuart K. Grange.
}
