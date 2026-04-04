\name{VaR.gpd.plots}

\alias{VaR.gpd.plots}

\title{Diagnostic Plots for VaR Calculation from GPD Approximation}

\description{
This function produces some diagnostic plots for VaR estimation using 
output of \code{VaR.gpd} function.
}
\usage{
VaR.gpd.plots(z)
}

\arguments{
  \item{z}{Output of \code{VaR.gpd} function}
}

\details{
Returns plots of daily return (\%), fit of sample distribution, quantile plot, 
loglikelihood functions for VaR and ES.
}

\author{T. Daniyarov}

\seealso{\code{\link{VaR.gpd}}}

\examples{
data(exchange.rates)
attach(exchange.rates)
y <- USDJPY[!is.na(USDJPY)]
z <- VaR.gpd(y)
VaR.gpd.plots(z)
detach(exchange.rates)
}
\keyword{aplot}
