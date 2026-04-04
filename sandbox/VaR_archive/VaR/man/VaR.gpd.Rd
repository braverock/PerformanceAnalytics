\name{VaR.gpd}
\alias{VaR.gpd}

\title{Value at Risk Calculation from Log-Likelihood Fit of General Pareto Distribution (GPD)}

\description{
Estimation of Value at Risk from log-likelihood fit of GPD.
}

\usage{
VaR.gpd(ydat, p = 0.01, p.tr = 0.97, drift.appx = FALSE, init = c(1, 0.3), cflevel = 0.95)
}

\arguments{
  \item{ydat}{Numeric vector of data for which VaR is to be calculated.}
  \item{p}{Confidence level for VaR calculation.}
  \item{p.tr}{Threshold for GPD fit.}
  \item{drift.appx}{ Logical; if \code{TRUE} VaR is calculated in non-zero drift approximation.}
  \item{init}{ Initial values for log-likelihood fit of GPD.}
  \item{cflevel}{Confidence level for estimation of VaR and ES intervals.}
}

\details{
This function estimates Value at Risk and Expected Shortfall of a single risk factor with a given confidence by using a fit of Generalized
Pareto Distribution to the part of data exceeding a given threshold (Peak over Threshold (POT) Method). The input data transformed
to procentual daily return. Then, transformed data is sorted and only part exceeding a given threshold is hold. Threshold is calculated
according an expression \code{p.tr*std}. Log-likelihood fit is then applied to get values of VaR and ES. After that, confidence
intervals for this values are calculated (see reference for details).
}

\value{    
 A list containing following components:
  \item{VaR}{Value at Risk for input data.}
  \item{VaR.interval}{Lower and higher bounds of VaR estimation with confidence given by parameter \code{cflevel}.}
  \item{ES}{Expected shortfall.}
  \item{ES.interval}{Lower and higher bounds of ES estimation with confidence given by parameter \code{cflevel}.}
  \item{data}{Same as \code{ydat}.}
  \item{cdata}{Vector of data used for GPD fit.}  
  \item{conf.level}{Same as \code{p}.}
  \item{tr}{Same as \code{p.tr}.}
  \item{mean}{Mean value of \code{cdata}.}  
  \item{std}{Standard deviation of \code{cdata}.}
  \item{gfit}{Best fit values of GPD.}
  \item{int.conf.level}{Same as \code{cflevel}.}
}

\references{Embrechts, P., Kluepelberg, C., and Mikosch, T. (1999) Modelling
Extremal Events for Insurance and Finance. Application of Mathematics. Springer.
2nd ed. (1st ed., 1997)}

\author{T. Daniyarov}

\seealso{\code{\link{VaR.gpd.plots}}}

\examples{
data(exchange.rates)
attach(exchange.rates)
y <- USDJPY[!is.na(USDJPY)]
z <- VaR.gpd(y)
z$VaR
z$VaR.interval
z$ES
z$ES.interval
detach(exchange.rates)
}

\keyword{ts}% at least one, from doc/KEYWORDS
