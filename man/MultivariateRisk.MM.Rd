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
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
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
  \item{w}{ ~~Describe \code{w} here~~ }
  \item{mu}{ ~~Describe \code{mu} here~~ }
  \item{sigma}{ ~~Describe \code{sigma} here~~ }
  \item{M3}{ The third mathematical moment of the joint distribution, provided by  \code{\link{M3.MM}} }
  \item{M4}{ The third mathematical moment of the joint distribution, provided by  \code{\link{M3.MM}} }
  \item{p}{ confidence level for calculation, default p=.99 }
}
\details{
  ~~ If necessary, more details than the description above ~~
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
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
