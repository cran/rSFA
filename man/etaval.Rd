% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eta.R
\name{etaval}
\alias{etaval}
\title{Computes the eta value of a signal (slowness)}
\usage{
etaval(x, T = length(x))
}
\arguments{
\item{x}{The columns of signal correspond to different input components. Must be normalized (zero mean, unit variance)}

\item{T}{Time interval}
}
\value{
returns the eta value of the signal in a time interval T time units long.
}
\description{
Computes the eta value of a signal (slowness)
}
