\name{sfaClassPredict}
\alias{sfaClassPredict}
\title{Predict Class for SFA classification}
\usage{
  sfaClassPredict(xtst, realcTst, opts)
}
\arguments{
  \item{xtst}{NTST x IDIM, test input data}

  \item{realcTst}{1 x NTST, test class labels}

  \item{opts}{list with several parameter settings:\cr
  gaussdim\cr ...\cr *Filename [* = s,g,x] from where to
  load the models (see sfaClassModel)}
}
\value{
  list \code{res} \cr - \code{res$errtst} 1 x 2 matrix:
  error rate with / w/o SFA on test set - \code{res$ytst}
  output from SFA when applied to test data -
  \code{res$predT} predictions with SFA + GaussClassifier
  on test set - \code{res$predX} predictions w/o SFA (only
  GaussClassifier) on test set (only if opts.xFilename
  exists)
}
\description{
  Use a SFA classification model (stored in
  opts$*Filename), predict & evaluate on new data
  (xtst,realc_tst).\cr Author of orig. matlab version:
  Wolfgang Konen, Jan 2011-Mar 2011.\cr See also [Berkes05]
  Pietro Berkes: Pattern recognition with Slow Feature
  Analysis. Cognitive Sciences EPrint Archive (CogPrint)
  4104, http://cogprints.org/4104/ (2005)
}
\references{
  \code{\link{sfaClassify}} \code{\link{sfaExecute}}
}

