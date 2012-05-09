\name{sfaNlRegress}
\alias{sfaNlRegress}
\title{Perform non-linear regression}
\usage{
  sfaNlRegress(sfaList, arg, func)
}
\arguments{
  \item{sfaList}{A list that contains all information about
  the handled sfa-structure}

  \item{arg}{Input data, each column a different variable}

  \item{func}{(T x 1) the function to be fitted
  nonlinearly}
}
\value{
  returns a list \code{res} with elements\cr \item{res$R}{
  (T x 1) the function fitted by NL-regression }
  \item{res$rcoef}{ (M x 1) the coefficients for the
  NL-expanded dimensions }
}
\description{
  Given the data in arg, expand them nonlinearly in the
  same way as it was done in the SFA-object sfaList
  (expanded dimension M) and search the vector RCOEF of M
  constant coefficients, such that the sum of squared
  residuals between a given function in time FUNC and the
  function\cr R(t) = (v(t) - v0)' * RCOEF, t=1,...,T,\cr is
  minimal
}

