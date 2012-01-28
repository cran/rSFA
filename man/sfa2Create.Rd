\name{sfa2Create}
\alias{sfa2Create}
\title{Create structured list for expanded SFA}
\usage{
  sfa2Create(ppRange, sfaRange, ppType = "SFA1",
    axType = "ORD1", regCt = 0, opts = NULL,
    xpDimFun = xpDim, sfaExpandFun = sfaExpand)
}
\arguments{
  \item{ppRange}{number of dimensions to be kept after
  preprocessing step}

  \item{sfaRange}{number of slowly-varying functions to be
  kept}

  \item{ppType}{preprocessing type: ="PCA" (principal
  component analysis) or ="SFA1" (linear sfa)}

  \item{axType}{is the type of derivative approximation to
  be used, see: \code{\link{sfaTimediff}}}

  \item{regCt}{regularization constant is not used at the
  moment}

  \item{opts}{optional list of additional options}

  \item{xpDimFun}{Function to calculate dimension of
  expanded data}

  \item{sfaExpandFun}{Function to expand data}
}
\value{
  list \code{sfaList} contains all settings for sfa2\cr
}
\description{
  Create structured list for expanded SFA
}
\references{
  \code{\link{sfa2}} \code{\link{sfaStep}}
  \code{\link{sfa1Create}}
}

