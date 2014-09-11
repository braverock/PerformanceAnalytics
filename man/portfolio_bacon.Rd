\name{portfolio_bacon}
\docType{data}
\alias{portfolio_bacon}
\title{Bacon(2008) Data}
\description{
A xts object that contains columns of monthly returns for an example of portfolio
and its benchmark
}
\usage{portfolio_bacon}
\format{CSV conformed into an xts object with monthly observations}
\examples{
data(portfolio_bacon)

#preview the data
head(portfolio_bacon)

#summary period statistics
summary(portfolio_bacon)

#cumulative returns
tail(cumprod(1+portfolio_bacon),1)
}
\keyword{datasets}
