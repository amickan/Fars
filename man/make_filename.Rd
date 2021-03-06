% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Creates a filename based on a specific year.}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A numeric vector representing the years in a time period.}
}
\value{
A character vector corresponding to the filenames for the specified time period
}
\description{
It creates the filename string using the format of the US National Highway
Traffic Safety Administration in the Fatality Analysis Reporting System for
a specific year.
}
\section{Warning}{

   If the \code{year} parameter cannot be coerced as an integer, an error will be thrown.
}

\examples{
\dontrun{
Creating the filename for year 2013:
make_filename(2013)}
}
