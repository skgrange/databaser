% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_simple_wrappers.R
\name{db_with_transaction}
\alias{db_with_transaction}
\title{Function open a database transaction, perform operations, and commit the 
results if a series of successful operations are performed.}
\usage{
db_with_transaction(con, code, ...)
}
\arguments{
\item{con}{Database connection.}

\item{code}{R code that interacts with \code{con}.}

\item{...}{Other parameters passed on to methods.}
}
\value{
Invisible logical vector.
}
\description{
Function open a database transaction, perform operations, and commit the 
results if a series of successful operations are performed.
}
\seealso{
\code{\link{dbWithTransaction}}
}
