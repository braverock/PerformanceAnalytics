\name{VaR.norm}

\alias{VaR.norm}

\title{Value at Risk Calculation in Lognormal Approximation}

\description{This function estimates Value of Risk (VaR) value in lognormal approximation.}

\usage{
VaR.norm(ydat, p = 0.99, dt = 1, type = "long", drift.appx = FALSE, lin.appx = TRUE)
}

\arguments{
  \item{ydat}{Numeric vector of data for which VaR is to be calculated}
  \item{p}{Confidence level for VaR calculation}
  \item{dt}{Liquidation period}
  \item{type}{String describing type of VaR calculated: "long" or "short"}
  \item{drift.appx}{Logical; if \code{TRUE} VaR is calculated in non-zero drift approximation }
  \item{lin.appx}{Logical; if \code{TRUE} VaR is calculated in linear approximation }
}

\details{
This function estimates VaR for a single risk factor \eqn{S(t)} in lognormal approximation. 
The final expression for VaR of {\bf long} and {\bf short} position is 
\deqn{VaR_{long}(c)=S(t)[1-exp(\mu \delta t + Q^{N(0,1)}_{1-c} \sigma \sqrt{\delta t})]}
\deqn{VaR_{short}(c)=-S(t)[1-exp(\mu \delta t - Q^{N(0,1)}_{1-c} \sigma \sqrt{\delta t})]}
Here, \eqn{c} is a desired confidence, \eqn{Q^{N(0,1)}_{1-c}} is a \eqn{1-c} percentile of normal
distribution, \eqn{\delta t} is liquidation period, and parameters \eqn{\mu} and \eqn{\sigma} are
mean value (or drift) and standard deviation of \eqn{\delta S(t)}.
If \code{drift.appx}=\code{FALSE}, \eqn{\mu = 0}. If \code{lin.appx}=\code{TRUE}, the above functions are expanded 
according \eqn{exp(x) = 1+x}.
}

\value{
  Return value is a list containing following components:
  \item{VaR}{Value at Risk for input data}
  \item{data}{Input data}
  \item{cdata}{Log-transformed data}
  \item{liq.period}{Same as \code{dt}}
  \item{type}{Same as \code{type}}
  \item{conf.level}{Same as \code{p}}
  \item{mean}{Mean value of \code{cdata}}
  \item{std}{Standard deviation of \code{cdata}}
}

\references{ Deutsch, H.P., Derivatives and Internal Models, 2nd Edition, Palgrave, London 2001}

\author{T. Daniyarov}

\seealso{ \code{\link{VaR.norm.plots}}, \code{\link{VaR.backtest}}}

\examples{
data(exchange.rates)
attach(exchange.rates)
y <- USDJPY[!is.na(USDJPY)]
z <- VaR.norm(y)
z$VaR
detach(exchange.rates)
}

\keyword{ts}
