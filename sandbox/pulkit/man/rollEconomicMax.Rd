\name{rollEconomicMax}
\alias{rollEconomicMax}
\title{Rolling Economic Max}
\usage{
  rollEconomicMax(R, Rf, h, geometric = TRUE, ...)
}
\arguments{
  \item{R}{R an xts, vector, matrix, data frame,
  timeseries, or zoo object of asset return.}

  \item{Rf}{risk free rate can be vector such as government
  security rate of return.}

  \item{h}{Look back period}

  \item{geomtric}{geometric utilize geometric chaining
  (TRUE) or simple/arithmetic #'chaining(FALSE) to
  aggregate returns, default is TRUE.}

  \item{...}{any other variable}
}
\description{
  Rolling Economic Max at time t, looking back at portfolio
  Wealth history for a rolling window of length H is given
  by:

  \deqn{REM(t,h)=\max_{t-H \leq s}\[(1+r_f)^{t-s}W_s\]}

  Here rf is the average realized risk free rate over a
  period of length t-s. If the risk free rate is changing.
  This is used to compound.

  \deqn{ \prod_{i=s}^{t}(1+r_{i}.{\triangle}t)}

  here \eqn{r_i} denotes the risk free interest rate during
  \eqn{i^{th}} discrete time interval \eqn{{\triangle}t}.
}
\examples{
data(edhec)
rollEconomicMax(edhec,0.08,100)
}
\author{
  Pulkit Mehrotra
}
\seealso{
  \code{\link{chart.REDD}} \code{\link{EconomicDrawdown}}
  \code{\link{rollDrawdown}} \code{\link{REDDCOPS}}
  \code{\link{EDDCOPS}}
}

