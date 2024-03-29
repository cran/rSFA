% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sfaHelperFunctions.R
\name{sfaBSh}
\alias{sfaBSh}
\title{Backslash operator.}
\usage{
sfaBSh(X, Y)
}
\arguments{
\item{X}{X matrix}

\item{Y}{Y vector}
}
\value{
Returns coefficients
}
\description{
Reproduce what MATLAB's backslash operator can do, using qr() and qr.coef().
}
\keyword{internal}
