\name{sfa2Step}
\alias{sfa2Step}
\title{A step in the SFA2 algorithm.}
\usage{
  sfa2Step(sfaList, arg = NULL, step = NULL, method = NULL)
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
  \code{sfaList$step="sfa"} the choices are "SVDSFA"
  (recommended) or "GENEIG" (unstable).  GENEIG is not
  implemented in the current version, since R lacks the
  option to calculate generalized eigenvalues easily.}
}
\value{
  list \code{sfaList} taken from the input, with new
  information added to this list.  Among the new items are:
  \item{avg0}{ mean vector in input space} \item{avg1}{
  mean vector in expanded space} \item{W0}{ (ppRange x
  ppRange)-matrix, the whitening matrix for the input data}
  \item{C}{ covariance matrix of the time-diff of expanded
  and sphered data} \item{SF}{ (sfaRange x sfaRange)-matrix
  with rows which contain the directions in expanded space
  with slow signals. The rows are sorted acc. to increasing
  eigenvalues of C}
}
\description{
  !!! Do not use this function directly, use sfaStep
  instead !!!
}
\seealso{
  \code{\link{sfaStep}} \code{\link{sfa2Create}}
  \code{\link{sfa1Step}}
}
\keyword{internal}

