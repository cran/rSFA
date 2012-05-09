\name{sfaPreproc}
\alias{sfaPreproc}
\title{Preprocessing for SFA classification}
\usage{
  sfaPreproc(sfaList, x, opts)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{x}{Input data, each column a different variable}

  \item{opts}{list}
}
\value{
  preprocessed data, which is \code{sfaList$W0 %*% (x -
  sfaList$avg0) }
}
\description{
  Helper function for \code{\link{sfaClassify}}
}
\seealso{
  \code{\link{sfaClassPredict}} \code{\link{sfaClassify}}
}
\keyword{internal}

