\name{sfaTimediff}
\alias{sfaTimediff}
\title{Calculates the first derivative of signal data}
\usage{
  sfaTimediff(DATA, axType = "ORD1")
}
\arguments{
  \item{DATA}{The matrix of signals for which the
  derivative is calculated (one column per signal)}

  \item{axType}{Type of interpolation: "ORD1" (default)
  first order, "SCD" second ,"TRD" third, "ORD3a" cubic
  polynom}
}
\value{
  matrix \code{DATA} \cr - \code{DATA} contains the
  derivative signals, with the same structure as the input
  data.
}
\description{
  Calculates the first derivative of signal data
}
\note{
  setting axType to invalid values will lead to first order
  interpolation.
}

