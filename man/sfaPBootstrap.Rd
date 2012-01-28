\name{sfaPBootstrap}
\alias{sfaPBootstrap}
\title{Parametric Bootstrap}
\usage{
  sfaPBootstrap(realclass, x, opts)
}
\arguments{
  \item{realclass}{true class of training data (can be
  vector, numerics, integers, factors)}

  \item{x}{matrix containing the training data}

  \item{opts}{list with several parameter settings\cr
  \code{opts$xpDimFun} (=xpDim by default) calculated
  dimension of expaned SFA space\cr \code{opts$deg} degree
  of expansion (should not be 1, not implemented)
  \code{opts$ppRange} ppRange for SFA algorithm
  \code{opts$nclass} number of unique classes
  \code{opts$doPB} do (1) or do no (0) param. bootstrap.}
}
\value{
  returns a code{list} \cr - code{list} contains: \cr
  \code{x} training set extended to minimu number of
  recors1.5*(xpdim+nclass), if necessary \code{realclass}
  training class labels, extended analogously
}
\description{
  If training set too small, augment it with parametric
  bootstrap
}
\references{
  \code{\link{addNoisyCopies}}
}

