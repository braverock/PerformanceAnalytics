\name{REDDCOPS}
\alias{REDDCOPS}
\title{Rolling Economic Drawdown Controlled Optimal Portfolio Strategy}
\usage{
  REDDCOPS(R, delta, Rf, h, geometric = TRUE,
    asset = c("one", "two", "three"),
    type = c("calibrated", "risk-based"), sharpe = 0, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{delta}{Drawdown limit}

  \item{sharpe}{If you want to use a constant Sharpe Ratio
  please specify here else the return series will be used}

  \item{Rf}{risk free rate can be vector such as government
  security rate of return.}

  \item{h}{Look back period}

  \item{geometric}{geometric utilize geometric chaining
  (TRUE) or simple/arithmetic chaining(FALSE) to aggregate
  returns, default is TRUE.}

  \item{\dots}{any other variable}

  \item{asset}{The number of risky assets in the portfolio}

  \item{type}{The type of portfolio optimization}
}
\description{
  The Rolling Economic Drawdown Controlled Optimal
  Portfolio Strategy(REDD-COPS) has the portfolio fraction
  allocated to single risky asset as:

  The risk free asset accounts for the rest of the
  portfolio allocation \eqn{x_f = 1 - x_t}.

  For two risky assets in REDD-COPS,dynamic asset
  allocation weights are :

  The portion of the risk free asset is \eqn{x_f = 1 - x_1
  - x_2}. dt<-read.zoo("../data/ret.csv",sep=",",header =
  TRUE)
}
\examples{
# with S&P 500 data and T-bill data
dt<-data(ret)
dt<-as.xts(read.zoo(ret))
REDDCOPS(dt[,1],delta = 0.33,Rf = (1+dt[,3])^(1/12)-1,h = 12,geometric = TRUE,asset = "one")


# with S&P 500 , barclays and T-bill data
data(ret)
dt<-as.xts(read.zoo(ret))
REDDCOPS(dt[,1:2],delta = 0.33,Rf = (1+dt[,3])^(1/12)-1,h = 12,geometric = TRUE,asset = "two")

data(edhec)
REDDCOPS(edhec,delta = 0.1,Rf = 0,h = 40)
data(managers)
REDDCOPS(managers[,1],0.80, Rf = managers[,10,drop=FALSE],12,asset="one")
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
  \code{\link{rollDrawdown}} \code{\link{EDDCOPS}}
  \code{\link{rollEconomicMax}}
}

