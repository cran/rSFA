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

  \item{method}{Method to be used: "SVDSFA" (recommended)
  or "GENEIG" (unstable) for step="sfa" \cr Else use
  "TIMESERIES" or "CLASSIF".  GENEIG is not implemented in
  the current version, since R lacks the option to
  calculate generalized eigenvalues easily.}
}
\value{
  list \code{sfaList} \cr - \code{sfaList} like the input,
  new information added to this list.
}
\description{
  !!! Do not use this function directly, use sfaStep
  instead !!!
}
\references{
  \code{\link{sfaStep}} \code{\link{sfa2Create}}
  \code{\link{sfa1Step}}
}
\keyword{internal}

