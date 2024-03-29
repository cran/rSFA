% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfaHelperFunctions.R
\name{customSize}
\alias{customSize}
\title{Custom Size Function.}
\usage{
customSize(x, i = NULL)
}
\arguments{
\item{x}{object to be checked for size}

\item{i}{1, 2 or NULL. Defines if both or which size information should be returned.}
}
\value{
a vector if i is null, an integer if i is 1 or 2.
}
\description{
custom R version of matlabs size function. Calls length for vectors, or else calls dim.
}
\keyword{internal}
