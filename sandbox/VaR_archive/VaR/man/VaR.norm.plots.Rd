\name{VaR.norm.plots}
\alias{VaR.norm.plots}

\title{Diagnostic Plots for VaR Calculation in Lognormal Approximation}

\description{
This function produces some diagnostic plots for VaR calculation in lognormal 
approximation.
}

\usage{
VaR.norm.plots(z)
}

\arguments{
  \item{z}{ An object returned by \code{VaR.norm()} function }
}

\details{
Returns plots of daily log return and of daily log return histogram with
the best fit provided by \code{VaR.norm()}.	 
}

\author{T. Daniyarov}


\seealso{\code{\link{VaR.norm}}}

\examples{
data(exchange.rates)
attach(exchange.rates)
y <- USDJPY[!is.na(USDJPY)]
z <- VaR.norm(y)
VaR.norm.plots(z)
detach(exchange.rates)
}
\keyword{ aplot}
