\name{sfaExpand}
\alias{sfaExpand}
\title{Degree 2 Expansion}
\usage{
  sfaExpand(sfaList, DATA)
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
  Expand a signal in the space of polynomials of degree 2.
  This is the default expansion function used by rSFA.
}
\seealso{
  \code{\link{sfa2}} \code{\link{nlExpand}}
  \code{\link{xpDim}}
}

