\name{sfaPBootstrap}
\alias{sfaPBootstrap}
\title{Parametric Bootstrap}
\usage{
  sfaPBootstrap(realclass, x, sfaList)
}
\arguments{
  \item{realclass}{true class of training data (can be
  vector, numerics, integers, factors)}

  \item{x}{matrix containing the training data}

  \item{sfaList}{list with several parameter settings, e.g.
  as created by \code{\link{sfa2Create}}\cr
  \code{sfaList$xpDimFun} (=xpDim by default) calculated
  dimension of expaned SFA space\cr \code{sfaList$deg}
  degree of expansion (should not be 1, not implemented)\cr
  \code{sfaList$ppRange} ppRange for SFA algorithm\cr
  \code{sfaList$nclass} number of unique classes\cr
  \code{sfaList$doPB} do (1) or do no (0) param.
  bootstrap.}
}
\value{
  a list \code{list} containing: \item{x}{ training set
  extended to minimu number of recors1.5*(xpdim+nclass), if
  necessary } \item{realclass}{ training class labels,
  extended analogously }
}
\description{
  If training set too small, augment it with parametric
  bootstrap
}
\seealso{
  \code{\link{addNoisyCopies}}
}

