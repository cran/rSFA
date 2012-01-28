\name{sfa2}
\alias{sfa2}
\title{The SFA2 algorithm, degree 2 expansion.}
\usage{
  sfa2(x, method = "SVDSFA", ppType = "PCA",
    xpDimFun = xpDim, sfaExpandFun = sfaExpand)
}
\arguments{
  \item{x}{Input data}

  \item{method}{="SVDSFA" for singular value decomposition
  (recommended) or ="GENEIG" for generalized eigenvalues
  (unstable!)  GENEIG is not implemented in the current
  version, since R lacks the option to calculate
  generalized eigenvalues easily.}

  \item{ppType}{preprocessing type: ="PCA" (principal
  component analysis) or ="SFA1" (linear sfa)}

  \item{xpDimFun}{Function to calculate dimension of
  expanded data}

  \item{sfaExpandFun}{Function to expand data}
}
\value{
  list \code{sfaList} with all learned information, where
  \code{sfaList$y} contains the Y outputs
}
\description{
  Y = sfa2(X) performs expanded Slow Feature Analysis on
  the input data X and returns the output signals Y ordered
  by increasing temporal variation, i.e. the first signal
  Y[,1] is the slowest varying one, Y[,2] the next slowest
  varying one and so on. The input data have to be
  organized with each variable on a column and each data
  point on a row, i.e. X(t,i) is the value of variable nr.
  i at time t.  By Default an expansion to the space of 2nd
  degree polynomials is done, this can be changed by using
  different functions for xpDimFun and sfaExpandFun.
}
\examples{
## prepare input data for simple demo
t=seq.int(from=0,by=0.011,to=2*pi)
x1=sin(t)+cos(11*t)^2
x2=cos(11*t)
x=data.frame(x1,x2)
## perform sfa2 algorithm with data
res = sfa2(x)
## plot slowest varying function of result
plot(t, res$y[,1],type="l",main="output of the slowest varying function")
## see http://www.scholarpedia.org/article/Slow_feature_analysis#The_algorithm
## for detailed description of this example
}
\references{
  \code{\link{sfaStep}} \code{\link{sfa1Create}}
  \code{\link{sfaExecute}}
}

