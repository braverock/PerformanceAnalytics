\name{exchange.rates}
\alias{exchange.rates}
\docType{data}
\title{EUR/USD, USD/JPY, USD/CHF and GPD/USD Exchange Rates}
\description{
Data on the currencies exchange rates for USD, EUR, CHF, GPD. Data covers a
period from 03-01-2000 till 22-08-2003.
}
\usage{data(exchange.rates)}
\format{
  A data frame with 950 observations on the following 5 variables.
  \describe{
    \item{date}{a POSIXt object containing dates.}
    \item{EURUSD}{a numeric vector. EUR against USD exchange rate.}
    \item{USDJPY}{a numeric vector. USD against JPY exchange rate.}
    \item{USDCHF}{a numeric vector. USD against CHF exchange rate.}
    \item{GPDUSD}{a numeric vector. GPD against USD exchange rate.}
  }
}
\source{
\url{http://www.federalreserve.gov/releases/}
}
\examples{
data(exchange.rates)
attach(exchange.rates)
plot(date, USDJPY, xlab = "Time", ylab = "Exchange Rate", main = "USD/JPY Exchange Rate", type = "l")
axis.POSIXct(1, date)
detach(exchange.rates)
}
\keyword{datasets}
