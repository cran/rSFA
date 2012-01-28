\name{sfaStep}
\alias{sfaStep}
\title{Update a step of the SFA algorithm.}
\usage{
  sfaStep(sfaList, arg, step = NULL, method = NULL)
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
  "TIMESERIES" or "CLASSIF". Irrelevant if sfa1step is
  used.  GENEIG is not implemented in the current version,
  since R lacks the option to calculate generalized
  eigenvalues easily.}
}
\value{
  list \code{sfaList} \cr - \code{sfaList} like the input,
  new information added to this list.
}
\description{
  sfaStep() updates the current step of the SFA algorithm.
  Example: suppose you have divided your training data into
  two chunks, DATA1 and DATA2. Let the number of input
  dimensions be N. To apply SFA on them write:\cr sfaL =
  sfa2Create(N,xpDim(N))\cr sfaL = sfaStep(hdl, DATA1,
  "preprocessing")\cr sfaL = sfaStep(hdl, DATA2)\cr sfaL =
  sfaStep(hdl, DATA1, "expansion")\cr sfaL = sfaStep(hdl,
  DATA2)\cr sfaL = sfaStep(hdl, NULL, "sfa")\cr output1 =
  sfaExecute(hdl, DATA1)\cr output2 = sfaExecute(hdl,
  DATA2)
}
\references{
  \code{\link{sfa1Create}} \code{\link{sfa2Create}}
  \code{\link{sfaExecute}}
}

