% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.r
\name{fars_read_years}
\alias{fars_read_years}
\title{Create month-year-combinations of every accident for given years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{year as numeric or multiple years as numeric vector}
}
\value{
returns list of month/year-combinations with seperate tibble list
  element for every year
}
\description{
\code{fars_read_years()} computes the month-year-combination for every
accident found in the files, which are specified by the given \code{years}.
Function constitutes a helper function for further summary statistics. Throws
an error if file for a given years does not exist.
}
\examples{
\dontrun{fars_read_years(2013)}
\dontrun{fars_read_years(c(2013, 2014))}
}
