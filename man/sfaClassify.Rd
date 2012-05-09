\name{sfaClassify}
\alias{sfaClassify}
\title{Predict Class for SFA classification}
\usage{
  sfaClassify(x, realclass, xtst = 0, realcTst = 0, opts)
}
\arguments{
  \item{x}{NREC x IDIM, training input data}

  \item{realclass}{1 x NREC, training class labels}

  \item{xtst}{NTST x IDIM, test input data}

  \item{realcTst}{1 x NTST, test class labels}

  \item{opts}{list with several parameter settings:
  \describe{ \item{gaussdim}{} \item{ ... }{} \item{
  *Filename}{ [* = s,g,x] from where to load the models
  (see \code{\link{sfaClassify}}) } }}
}
\value{
  list \code{res} containing \cr \item{res$errtrn}{ 1 x 2
  matrix: error rate with / w/o SFA on training set }
  \item{res$errtst}{ 1 x 2 matrix: error rate with / w/o
  SFA on test set } \item{res$y}{ output from SFA when
  applied to training data } \item{res$ytst}{ output from
  SFA when applied to test data } \item{res$predT}{
  predictions with SFA + GaussClassifier on test set }
  \item{res$predX}{ predictions w/o SFA (only
  GaussClassifier) on test set (only if opts.xFilename
  exists) }
}
\description{
  Create a SFA classification mode, predict & evaluate on
  new data (xtst,realc_tst).\cr Author of orig. matlab
  version: Wolfgang Konen, May 2009 - Jan 2010\cr See also
  [Berkes05] Pietro Berkes: Pattern recognition with Slow
  Feature Analysis. Cognitive Sciences EPrint Archive
  (CogPrint) 4104, http://cogprints.org/4104/ (2005)
}
\seealso{
  \code{\link{sfaClassPredict}} \code{\link{sfaExecute}}
}

