\name{sfaGetHf}
\alias{sfaGetHf}
\title{Return a SFA function as a quadratic form.}
\usage{
  sfaGetHf(sfaList, nr, where)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{nr}{function number}

  \item{where}{WHERE parameter}
}
\value{
  list \code{res} \cr - \code{res} contains: \code{res$H}
  \code{res$f} \code{res$c}
}
\description{
  sfaGetHf returns function number NR in the sfa object
  referenced by HDL in the form of a quadratic form\cr q(x)
  = 1/2*x'*H*x + f'*x + c\cr Of course, this only works if
  a quadratic expansion was used during training.  The
  quadratic form can lie in different spaces, i.e. it can
  receive as input preprocessed or non-preprocessed
  vectors. This is specified by setting the argument WHERE.
  The quadratic form lies - in the preprocessed space for
  WHERE==0 (e.g. the whitened space if the preprocessing
  type is PCA) - in the PCA space (i.e. projected on the
  principal components but not whitened, works only if PCA
  was used for preprocessing) for WHERE==1 - in the input,
  mean-free space for WHERE==2 - in the input space for
  WHERE==3 In general you will need to set WHERE to 2 or 3,
  but working in the preprocessed spaces can often
  drastically improve the speed of analysis.
}
\references{
  \code{\link{sfa2Create}}
}
\keyword{internal}

