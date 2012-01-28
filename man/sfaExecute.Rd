\name{sfaExecute}
\alias{sfaExecute}
\title{Execute learned function for input data}
\usage{
  sfaExecute(sfaList, DATA, prj = NULL, ncomp = NULL)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{DATA}{Input data, each column a different variable}

  \item{prj}{If not NULL, the preprocessing step 1 is
  skipped for SFA2}

  \item{ncomp}{number of learned functions to be used}
}
\value{
  matrix \code{DATA} containing the calculated output \cr
}
\description{
  After completion of the learning phase (step="sfa") this
  function can be used to apply the learned function to the
  input data. \cr The execution is completed in 4 steps:\cr
  1. projection on the input principal components
  (dimensionality reduction)\cr 2. expansion (if
  necessary)\cr 3. projection on the whitened (expanded)
  space\cr 4. projection on the slow functions
}
\references{
  \code{\link{sfa2}} \code{\link{sfa1}}
  \code{\link{sfaStep}}
}

