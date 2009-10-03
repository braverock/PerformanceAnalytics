\name{Return.excess}
\alias{Return.excess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculates the returns of an asset in excess of the given risk free rate}
\description{
Calculates the returns of an asset in excess of the given "risk free rate" for the period.

Ideally, your risk free rate will be for each period you have returns observations, but a single average return for the period will work too.
}
\usage{
Return.excess(R, Rf=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rf}{ risk free rate, in same period as your returns, or as a sinlge digit average }
}
\value{
mean excess return
}
\details{
Mean of the period return minus the period risk free rate

\deqn{\overline{(R_{a}-R_{f})}}{mean(Ra-Rf=0)}

OR

mean of the period returns minus a single numeric risk free rate

\deqn{\overline{R_{a}}-R_{f}}{mean(R)-rf}

Note that while we have, in keeping with common academic usage, assumed that the second parameter will be a risk free rate, you may also use any other timeseries as the second argument.  A common alteration would be to use a benchmark to produce excess returns over a specific benchmark, as demonstrated in the examples below.

}
\references{
Bacon, Carl. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 47-52
}
\author{ Peter Carl }
\examples{
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
end = rownames(edhec[edhec.length,])

edhec.zoo = zoo(edhec,order.by=rownames(edhec))
rf.zoo = download.RiskFree(start = start, end = end)
sp.zoo = download.SP500PriceReturns(start = start, end = end)

time(edhec.zoo) = as.yearmon(time(edhec.zoo))
time(sp.zoo) = as.yearmon(time(sp.zoo))
time(rf.zoo) = as.yearmon(time(rf.zoo))

Return.excess(edhec.zoo[,"Funds.of.Funds",drop=FALSE],rf.zoo)
Return.excess(edhec.zoo[,"Long.Short.Equity",drop=FALSE],rf.zoo)
Return.excess(edhec.zoo[,"Funds.of.Funds",drop=FALSE],sp.zoo)
Return.excess(edhec.zoo[,"Long.Short.Equity",drop=FALSE],sp.zoo)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }