\name{lcovPca}
\alias{lcovPca}
\title{Principal Component Analysis on a covariance object}
\usage{
  lcovPca(lcov, dimRange = NULL)
}
\arguments{
  \item{lcov}{A list that contains all information about
  the handled covariance-structure}

  \item{dimRange}{A number or vector for dimensionality
  reduction: \cr if it is a number: only the first
  components 1:dimRange are kept (those with largest
  eigenvalues)\cr if it is a range: only the components in
  the range dimRange[1]..dimRange[2] are kept}
}
\value{
  returns a list: $W is the whitening matrix, $DW the
  dewhitening matrix and $D an array containing a list of
  the eigenvalues. $kvar contains the total variance kept
  in percent.
}
\description{
  Performs PCA _and_ whitening on the covariance object
  referenced by lcov.  CAUTION: can be numerically instable
  if covariance matrix is singular, better use LCOV_PCA2
  instead /W. Konen/
}
\note{
  lcovFix(lcov) has to be used before this function is
  applied
}
\references{
  \code{\link{lcovFix}} \code{\link{lcovPca2}}
}

