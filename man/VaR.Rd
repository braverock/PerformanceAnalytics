\name{VaR}
\alias{VaR}
\alias{VaR.CornishFisher}
\title{calculate various Value at Risk (VaR) measures}
\usage{
VaR(R = NULL, p = 0.95, ..., method = c("modified", "gaussian",
  "historical", "kernel"), clean = c("none", "boudt", "geltner"),
  portfolio_method = c("single", "component", "marginal"), weights = NULL,
  mu = NULL, sigma = NULL, m3 = NULL, m4 = NULL, invert = TRUE)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{p}{confidence level for calculation, default p=.95}

  \item{method}{one of "modified","gaussian","historical",
  "kernel", see Details.}

  \item{clean}{method for data cleaning through
  \code{\link{Return.clean}}. Current options are "none",
  "boudt", or "geltner".}

  \item{portfolio_method}{one of
  "single","component","marginal" defining whether to do
  univariate, component, or marginal calc, see Details.}

  \item{weights}{portfolio weighting vector, default NULL,
  see Details}

  \item{mu}{If univariate, mu is the mean of the series.
  Otherwise mu is the vector of means of the return series
  , default NULL, , see Details}

  \item{sigma}{If univariate, sigma is the variance of the
  series. Otherwise sigma is the covariance matrix of the
  return series , default NULL, see Details}

  \item{m3}{If univariate, m3 is the skewness of the
  series. Otherwise m3 is the coskewness matrix of the
  returns series, default NULL, see Details}

  \item{m4}{If univariate, m4 is the excess kurtosis of the
  series. Otherwise m4 is the cokurtosis matrix of the
  return series, default NULL, see Details}

  \item{invert}{TRUE/FALSE whether to invert the VaR
  measure.  see Details.}

  \item{\dots}{any other passthru parameters}
}
\description{
Calculates Value-at-Risk(VaR) for univariate, component,
and marginal cases using a variety of analytical methods.
}
\note{
The option to \code{invert} the VaR measure should appease
both academics and practitioners.  The mathematical
definition of VaR as the negative value of a quantile will
(usually) produce a positive number. Practitioners will
argue that VaR denotes a loss, and should be internally
consistent with the quantile (a negative number).  For
tables and charts, different preferences may apply for
clarity and compactness.  As such, we provide the option,
and set the default to TRUE to keep the return consistent
with prior versions of PerformanceAnalytics, but make no
value judgment on which approach is preferable.

The prototype of the univariate Cornish Fisher VaR function
was completed by Prof. Diethelm Wuertz.  All corrections to
the calculation and error handling are the fault of Brian
Peterson.
}
\section{Background}{
  This function provides several estimation methods for the
  Value at Risk (typically written as VaR) of a return
  series and the Component VaR of a portfolio. Take care to
  capitalize VaR in the commonly accepted manner, to avoid
  confusion with var (variance) and VAR (vector
  auto-regression).  VaR is an industry standard for
  measuring downside risk. For a return series, VaR is
  defined as the high quantile (e.g. ~a 95% or 99%
  quantile) of the negative value of the returns. This
  quantile needs to be estimated.  With a sufficiently
  large data set, you may choose to utilize the empirical
  quantile calculated using \code{\link{quantile}}.  More
  efficient estimates of VaR are obtained if a (correct)
  assumption is made on the return distribution, such as
  the normal distribution.  If your return series is skewed
  and/or has excess kurtosis, Cornish-Fisher estimates of
  VaR can be more appropriate.  For the VaR of a portfolio,
  it is also of interest to decompose total portfolio VaR
  into the risk contributions of each of the portfolio
  components.  For the above mentioned VaR estimators, such
  a decomposition is possible in a financially meaningful
  way.
}
\examples{
data(edhec)

    # first do normal VaR calc
    VaR(edhec, p=.95, method="historical")

    # now use Gaussian
    VaR(edhec, p=.95, method="gaussian")

    # now use modified Cornish Fisher calc to take non-normal distribution into account
    VaR(edhec, p=.95, method="modified")

    # now use p=.99
    VaR(edhec, p=.99)
    # or the equivalent alpha=.01
    VaR(edhec, p=.01)

    # now with outliers squished
    VaR(edhec, clean="boudt")

    # add Component VaR for the equal weighted portfolio
    VaR(edhec, clean="boudt", portfolio_method="component")
}
\author{
Brian G. Peterson and Kris Boudt
}
\references{
Boudt, Kris, Peterson, Brian, and Christophe Croux. 2008.
Estimation and decomposition of downside risk for
portfolios with non-normal returns. 2008. The Journal of
Risk, vol. 11, 79-103.

Cont, Rama, Deguest, Romain and Giacomo Scandolo.
Robustness and sensitivity analysis of risk measurement
procedures. Financial Engineering Report No. 2007-06,
Columbia University Center for Financial Engineering.

Denton M. and Jayaraman, J.D. Incremental, Marginal, and
Component VaR. Sunguard. 2004.

Epperlein, E., Smillie, A. Cracking VaR with kernels. RISK,
2006, vol.  19, 70-74.

Gourieroux, Christian, Laurent, Jean-Paul and Olivier
Scaillet.  Sensitivity analysis of value at risk. Journal
of Empirical Finance, 2000, Vol. 7, 225-245.

Keel, Simon and Ardia, David. Generalized marginal risk.
Aeris CAPITAL discussion paper.

Laurent Favre and Jose-Antonio Galeano. Mean-Modified
Value-at-Risk Optimization with Hedge Funds. Journal of
Alternative Investment, Fall 2002, v 5.

Martellini, Lionel, and Volker Ziemann.  Improved Forecasts
of Higher-Order Comoments and Implications for Portfolio
Selection. 2007. EDHEC Risk and Asset Management Research
Centre working paper.

Return to RiskMetrics: Evolution of a Standard
\url{http://www.riskmetrics.com/publications/techdocs/r2rovv.html}

Zangari, Peter. A VaR Methodology for Portfolios that
include Options. 1996. RiskMetrics Monitor, First Quarter,
4-12.

Rockafellar, Terry and Uryasev, Stanislav. Optimization of
Conditional VaR. The Journal of Risk, 2000, vol. 2, 21-41.
}
\seealso{
\code{\link{SharpeRatio.modified}} \cr
\code{\link{chart.VaRSensitivity}} \cr
\code{\link{Return.clean}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

