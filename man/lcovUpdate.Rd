% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcov.R
\name{lcovUpdate}
\alias{lcovUpdate}
\title{Update a covariance object}
\usage{
lcovUpdate(lcov, DATA)
}
\arguments{
\item{lcov}{A list that contains all information about the handled covariance-structure}

\item{DATA}{Input Data: must be oriented so that each column is a variable and each row a new measurement}
}
\value{
list containing the COVMTX, avg and tlen
}
\description{
Updates the covariance object
 referenced by lcov with a new chunk of data DATA.
}
\seealso{
\code{\link{lcovCreate}}
}
\keyword{internal}
