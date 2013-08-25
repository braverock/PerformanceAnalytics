\name{EDDCOPS}
\alias{EDDCOPS}
\title{Economic Drawdown Controlled Optimal Portfolio Strategy}
\usage{
  EDDCOPS(R, delta, gamma, Rf, geometric = TRUE, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{delta}{Drawdown limit}

  \item{gamma}{(1-gamma) is the investor risk aversion else
  the return series will be used}

  \item{Rf}{risk free rate can be vector such as government
  security rate of return.}

  \item{h}{Look back period}

  \item{geomtric}{geometric utilize geometric chaining
  (TRUE) or simple/arithmetic #'chaining(FALSE) to
  aggregate returns, default is TRUE.}

  \item{...}{any other variable}
}
\description{
  The Economic Drawdown Controlled Optimal Portfolio
  Strategy(EDD-COPS) has the portfolio fraction allocated
  to single risky asset as:

  \deqn{x_t = Max\left\{0,\biggl(\frac{\lambda/\sigma +
  1/2}{1-\delta.\gamma}\biggr).\biggl[\frac{\delta-EDD(t)}{1-EDD(t)}\biggr]\right\}}

  The risk free asset accounts for the rest of the
  portfolio allocation \eqn{x_f = 1 - x_t}.
  dt<-read.zoo("../data/ret.csv",sep=",",header = TRUE)
}
\examples{
# with S&P 500 data and T-bill data
data(ret)
dt<-as.xts(read.zoo(ret))
EDDCOPS(dt[,1],delta = 0.33,gamma = 0.7,Rf = (1+dt[,2])^(1/12)-1,geometric = TRUE)

data(edhec)
EDDCOPS(edhec,delta = 0.1,gamma = 0.7,Rf = 0)
}
\author{
  Pulkit Mehrotra
}
\references{
  Yang, Z. George and Zhong, Liang, Optimal Portfolio
  Strategy to Control Maximum Drawdown - The Case of Risk
  Based Dynamic Asset Allocation (February 25, 2012)
}
\seealso{
  \code{\link{chart.REDD}} \code{\link{EconomicDrawdown}}
  \code{\link{rollDrawdown}} \code{\link{REDDCOPS}}
  \code{\link{rollEconomicMax}}
}

