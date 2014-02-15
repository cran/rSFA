\name{lcovFix}
\alias{lcovFix}
\title{Fix a covariance object}
\usage{
  lcovFix(lcov)
}
\arguments{
  \item{lcov}{A list that contains all information about
  the handled covariance-structure}
}
\value{
  returns the fixed covariance list structure
}
\description{
  Computes the definitive covariance matrix and the average
  of the covariance object referenced by lcov after a
  series of update operations.
}
\seealso{
  \code{\link{lcovCreate}} \code{\link{lcovPca}}
}
\keyword{internal}

