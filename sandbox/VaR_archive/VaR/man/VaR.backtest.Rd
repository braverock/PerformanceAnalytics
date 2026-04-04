\name{VaR.backtest}
\alias{VaR.backtest}

\title{Backtest of VaR Estimation}

\description{ 
Test for given proportions on input data respective a given VaR. 
}

\usage{
VaR.backtest(x, VaR, p)
}

\arguments{
  \item{x}{ Numerical vector of observation.}
  \item{VaR}{ Number or numerical vector of the same length as \code{x}.}
  \item{p}{Confidence level of VaR estimation.}
}

\details{
This function performs the test for equal or given proportion (prop.test) on
input data. It calculates a part of observation exceeding a VaR and compares it
with a confidence level of VaR estimation.
}

\value{
Return value is a p.value of prop.test and can be interpretated in a usual
manner.
}

\author{T. Daniyarov}

\seealso{\code{\link{prop.test}}}

\examples{
data(exchange.rates)
attach(exchange.rates)
y <- USDJPY[!is.na(USDJPY)]
z <- VaR.norm(y)
VaR.backtest(z$cdata, z$VaR, p = 0.01)
detach(exchange.rates)
}

\keyword{htest}
