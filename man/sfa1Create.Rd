\name{sfa1Create}
\alias{sfa1Create}
\title{Create structured list for linear SFA}
\usage{
  sfa1Create(sfaRange, axType = "ORD1", regCt = 0)
}
\arguments{
  \item{sfaRange}{number of slowly-varying functions to be
  kept}

  \item{axType}{is the type of derivative approximation to
  be used, see: \code{\link{sfaTimediff}}}

  \item{regCt}{regularization constant is not used at the
  moment}
}
\value{
  list \code{sfaList} contains all settings for sfa1\cr
}
\description{
  Create structured list for linear SFA
}
\references{
  \code{\link{sfa1}} \code{\link{sfaStep}}
  \code{\link{sfa2Create}}
}

