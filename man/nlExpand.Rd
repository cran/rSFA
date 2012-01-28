\name{nlExpand}
\alias{nlExpand}
\title{Expand a signal in the for Nonlinear Expansion demo}
\usage{
  nlExpand(sfaList, DATA)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{DATA}{Input data, each column a different variable}
}
\value{
  expanded matrix \code{DATA}
}
\description{
  This is an optional expansion function, calculating the
  expanded data in [x; x^4].
}
\references{
  \code{\link{sfa2}} \code{\link{sfaExpand}}
  \code{\link{nlDim}}
}
\keyword{internal}

