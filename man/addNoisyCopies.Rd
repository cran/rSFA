\name{addNoisyCopies}
\alias{addNoisyCopies}
\title{Add noisy copies for parametric bootstrap}
\usage{
  addNoisyCopies(realclass, x, pars)
}
\arguments{
  \item{x}{a matrix containing the training data}

  \item{realclass}{true class of training data (can be
  vector, numerics, integers, factors)}

  \item{pars}{list of parameters:\cr \code{pars$ncopies}:
  Number of new records to add\cr \code{pars$ncsort}:
  Defines if training data should be sorted by class.
  Default is FALSE\cr \code{pars$ncsigma}: The noise in
  each column of x has the std.dev. pars$ncsigma*(standard
  deviation of column). Default Value: 0.8\cr
  \code{pars$ncmethod}: =1: each 'old' record from X in
  turn is the centroid for a new pattern;\cr =2: the
  centroid is the average of all records from the same
  class, the std.dev. is the same for all classes;\cr =3:
  centroid as in '2', the std.dev. is the std.dev. of all
  records from the same class (*recommended*)\cr}
}
\value{
  list \code{res} \cr - \code{res} contains two list
  entries: realclass and x (including added copies)
}
\description{
  Given training data X with true labels REALCLASS, add new
  records to X and REALCLASS, which are noisy copies of the
  training data.
}
\references{
  \code{\link{sfaPBootstrap}}
}

