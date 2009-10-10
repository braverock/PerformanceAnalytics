\name{MultivariateRisk}
\alias{MultivariateRisk}
\alias{mVaR.MM}
\alias{mES.MM}
\alias{GVaR.MM}
\alias{GES.MM}
\alias{SR.GES.MM}
\alias{SR.GVaR.MM}
\alias{SR.StdDev.MM}
\alias{SR.mES.MM}
\alias{SR.mVaR.MM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate common VaR and ES risk measures on a portfolio distribution }
\description{
Functions to calculate Guassian and modified VaR and Expected Shortfall.

Note that these functions generally expect the moments to be passed in directly; this was done to eliminate repetitive calculations in a portfolio optimization context.  We'll write "friendly" wrapper functions that take the return vector when we have time.
}
\usage{
GVaR.MM (w, mu, sigma, p)
mVaR.MM(w, mu, sigma, M3, M4, p)
SR.mVaR.MM (w, mu, sigma, M3, M4, p)
GES.MM (w, mu, sigma, p)
mES.MM(w, mu, sigma, M3, M4, p)
SR.mES.MM (w, mu, sigma, M3, M4, p)
SR.GVaR.MM (w, mu, sigma, p)
SR.GES.MM (w, mu, sigma, p)
SR.StdDev.MM (w, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{ a vector of weights to be applied to each asset in trhe joint distribution }
  \item{mu}{ mean or first centered moment of the distribution, normalley provided by  \code{\link{Return.centered}} }
  \item{sigma}{ variance or second centered moment of the distribution }
  \item{M3}{ The third mathematical moment of the joint distribution, provided by  \code{\link{M3.MM}} }
  \item{M4}{ The fourth mathematical moment of the joint distribution, provided by  \code{\link{M4.MM}} }
  \item{p}{ confidence level for calculation, default p=.99 }
}
\details{
These functions were first utilized in Boudt, Peterson, and Croux (2008), and have been subsequently used in our other research.

  ~~ Additional Details will be added to documentation as soon as we have time to write them. ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{
Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008. Estimation and Decomposition
  of Downside Risk for Portfolios with Non-Normal Returns. Journal of Risk. Winter.

Martellini, Lionel, and Volker Ziemann. 2007. Improved Forecasts of Higher-Order Comoments and Implications for Portfolio Selection. EDHEC Risk and Asset Management Research Centre working paper.

Ranaldo, Angelo, and Laurent Favre Sr. 2005. How to Price Hedge Funds: From Two- to
  Four-Moment CAPM. SSRN eLibrary.

Scott, Robert C., and Philip A. Horvath. 1980. On the Direction of Preference for Moments of
  Higher Order than the Variance. Journal of Finance 35(4):915-919.

 }
\author{ Kris Boudt, Brian Peterson }
\note{ ~~further notes~~
}
\seealso{ 
\code{\link{MultivariateMoments}}, \code{\link{centeredmoment}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (w, mu, sigma, M3, M4, p)
{
    skew = skewness.MM(w, sigma, M3)
    exkurt = kurtosis.MM(w, sigma, M4) - 3
    z = qnorm(1 - p)
    h = z + (1/6) * (z^2 - 1) * skew
    h = h + (1/24) * (z^3 - 3 * z) * exkurt - (1/36) * (2 * z^3 -
        5 * z) * skew^2
    return(-mean.MM(w, mu) - h * StdDev.MM(w, sigma))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
