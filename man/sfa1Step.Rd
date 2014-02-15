\name{sfa1Step}
\alias{sfa1Step}
\title{A step in the SFA1 algorithm.}
\usage{
  sfa1Step(sfaList, arg = NULL, step = NULL, method = NULL)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{arg}{Input data, each column a different variable}

  \item{step}{Specifies the current SFA step.  Must be
  given in the right sequence: for SFA1 objects:
  "preprocessing", "sfa"\cr for SFA2 objects:
  "preprocessing", "expansion", "sfa" Each time a new step
  is invoked, the previous one is closed, which might take
  some time.}

  \item{method}{Method to be used: For
  \code{sfaList$step="expansion"} the choices are
  "TIMESERIES" or "CLASSIF". \cr For
  \code{sfaList$step="sfa"} currently no choices.}
}
\value{
  list \code{sfaList} taken from the input, with new
  information added to this list.  Among the new items are:
  \item{avg0}{ mean vector in input space} \item{SF}{
  (sfaRange x sfaRange)-matrix with rows which contain the
  directions in expanded space with slow signals. The rows
  are sorted acc. to increasing eigenvalues of time-diff
  covariance matrix}
}
\description{
  !!! Do not use this function directly, use sfaStep
  instead !!!
}
\seealso{
  \code{\link{sfaStep}} \code{\link{sfa1Create}}
  \code{\link{sfa2Step}}
}
\keyword{internal}

