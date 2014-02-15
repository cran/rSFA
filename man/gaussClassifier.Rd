\name{gaussClassifier}
\alias{gaussClassifier}
\title{Classifier for SFA demos}
\usage{
  gaussClassifier(gauss, y, realC, method = "train")
}
\arguments{
  \item{gauss}{List created by gaussCreate. Contains also
  the elements: \describe{ \item{aligned}{ =0: do not align
  the Gaussian classifiers with axes, use full covariance
  matrix \cr =1 (default): set the off-diagonals in
  covariance matrix to 0, i.e. the Gaussian classifier is
  forced to be aligned with the axes. This is more robust
  in the case where the data deviate largely from a
  multivariate normal distribution.  } \item{epsD}{
  [defaults to 0.04] replace diagonal elements of COV
  smaller than epsD with epsD to avoid too small Gaussians
  } }}

  \item{y}{K x M matrix where K is the total number of
  patterns and M is the number of variables used for
  classification. I.e. each row of y contains the data for
  one pattern.}

  \item{realC}{1 x K matrix with NCLASS distinct real class
  labels needed only for method='train'. In case of
  method="apply" realC is not used and can have any value}

  \item{method}{either "train" (default) or "apply"}
}
\value{
  list \code{gauss} containing \item{gauss$predC}{ 1 x K
  matrix: the predicted class } \item{gauss$prob}{ K x
  NCLASS matrix: prob(k,n) is the estimated probability
  that pattern k belongs to class m }
}
\description{
  Train or apply a Gaussian classifier..
}
\seealso{
  \code{\link{gaussCreate}}
}

