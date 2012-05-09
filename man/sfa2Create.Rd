\name{sfa2Create}
\alias{sfa2Create}
\title{Create structured list for expanded SFA}
\usage{
  sfa2Create(ppRange, sfaRange, ppType = "SFA1",
    axType = "ORD1", regCt = 0, opts = NULL,
    xpDimFun = xpDim, sfaExpandFun = sfaExpand)
}
\arguments{
  \item{ppRange}{umber of dimensions to be kept after
  preprocessing step - or - a two-number vector with lower
  and upper dimension number}

  \item{sfaRange}{umber of slowly-varying functions to be
  kept}

  \item{ppType}{preprocessing type: ="PCA", "PCA2"
  (principal component analysis) or ="SFA1" (linear sfa)}

  \item{axType}{is the type of derivative approximation to
  be used, see \code{\link{sfaTimediff}}}

  \item{regCt}{regularization constant, currently not used}

  \item{opts}{optional list of additional options}

  \item{xpDimFun}{Function to calculate dimension of
  expanded data}

  \item{sfaExpandFun}{Function to expand data}
}
\value{
  list \code{sfaList} contains all arguments passed into
  sfa2create plus \item{xpRange}{ evaluates to
  \code{xpDimFun(ppRange)} } \item{deg}{ 2} This list will
  be expanded by other SFA functions with further SFa
  results
}
\description{
  'Expanded' SFA means that the input data are expanded
  into a higher-dimensional space with the function
  sfaExpandFun. See \code{\link{sfaExpand}} for the default
  expansion function.
}
\seealso{
  \code{\link{sfa2}} \code{\link{sfaStep}}
  \code{\link{sfa1Create}}
}

