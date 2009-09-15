\name{VaR}
\alias{VaR}
\title{ calculate various Value at Risk (VaR) measures }
\description{
Calculates Value-at-Risk(VaR) for univariate, component, and marginal cases using a variety of analytical methods.
}
\usage{
VaR(R , p=0.99, method=c("modified","gaussian","historical", "kernel"), clean=c("none","boudt"),  portfolio_method=c("single","component","marginal"), weights=NULL, mu=NULL, sigma=NULL, skew=NULL, exkurt=NULL, m1=NULL, m2=NULL, m3=NULL, m4=NULL, invert=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{method}{ one of "modified","gaussian","historical", "kernel", see Details. }
  \item{clean} { method for data cleaning through \code{\link{Return.clean}}.  Current options are "none" or "boudt". }
  \item{portfolio_method}{ one of "single","component","marginal" defining whether to do univariate, component, or marginal calc, see Details. }
  \item{weights}{ portfolio weighting vector, default NULL, see Details}
  \item{mu}    { portfolio mu, default NULL, , see Details}
  \item{sigma} { portfolio sigma matrix, default NULL, see Details}
  \item{skew}  { portfolio skewness matrix, default NULL, see Details}
  \item{exkurt}{ portfolio excess kurtosis matrix, default NULL, see Details}
  \item{m1}    { portfolio m1 matrix, default NULL, see Details}
  \item{m2}    { portfolio m2 matrix, default NULL, see Details}
  \item{m3}    { portfolio m3 matrix, default NULL, see Details}
  \item{m4}    { portfolio m4 matrix, default NULL, see Details}
  \item{invert}{ TRUE/FALSE whether to invert the VaR measure.  see Details. }
  
}
\details{
\section{Background}{
In the early 90's, academic literature started referring to \dQuote{value at risk}, typically written as VaR. Take care to capitalize VaR in the commonly accepted manner, to avoid confusion with var (variance) and VAR (vector auto-regression).  With a sufficiently large data set, you may choose to utilize a non-parametric VaR estimation method using the historical distribution and the probability quantile of the distribution calculated using \code{\link{quantile}} for an arbitrary historical distribution or \code{\link{qnorm}} for an idealized normal distribution. The negative return at the correct quantile (usually 95\% or 99\%), is the non-parametric historical VaR estimate.
}
\section{Methods}{
In a set of returns for which sufficently long history exists, the per-period Value at Risk is simply the correct quantile of the period losses \eqn{\mu}  with variance \eqn{\sigma}  is denoted by:

\deqn{VaR=\mu_{q_{.99}}}
where \eqn{q_{.99}} is the 99\% quantile of the distribution.

This method is also sometimes called \dQuote{historical VaR}, as it is by definition \emph{ex post} analysis of the return distribution, and may be accessed with \code{method="historical"}.

When you don't have a sufficiently long set of returns to use non-parametric or historical VaR, or wish to more closely model an ideal distribution, it is common to us a parmetric estimate based on the distribution. J.P. Morgan's RiskMetrics parametric mean-VaR was published in 1994 and this methodology for estimating parametric mean-VaR has become what most literature generally refers to as \dQuote{VaR} and what we have implemented as \code{\link{VaR.traditional}}. See \cite{Return to RiskMetrics: Evolution of a Standard}\url{http://www.riskmetrics.com/r2rovv.html},

Parametric mean-VaR does a better job of accounting for the tails of the distribution by more precisely estimating shape of the distribution tails of the risk quantile. The most common estimate is a normal (or Gaussian) distribution \eqn{R\sim N(\mu,\sigma^{2})} for the return series. In this case, estimation of VaR requires the mean return \eqn{\bar{R}}, the distribution of losses \eqn{\mu}, and the variance of the returns \eqn{\sigma}. In the most common case, parametric VaR is thus calculated by

\deqn{\sigma=variance(R)}

\deqn{VaR=\bar{R} -  \sigma \cdot q_{c} }

where \eqn{q_{c}} is the $c$\% quantile of the distribution. Represented in \R by \code{qnorm(1-p)}, and may be accessed with \code{method="gaussian"}.

Other forms of parametric mean-VaR estimation utilize a different distribution for the distribution of losses \eqn{\mu} to better account for the possible fat-tailed nature of downside risk. The package \code{VaR} contains methods for simulating and estimating lognormal \code{\link[VaR]{VaR.norm}} and generalized Pareto \code{\link[VaR]{VaR.gpd}} distributions to overcome some of the problems with nonparametric or parametric mean-VaR calculations on a limited sample size or on potentially fat-tailed distributions.  There is also a \code{\link[VaR]{VaR.backtest}} function to apply simulation methods to create a more robust estimate of the potential distribution of losses. Less commonly a covariance matrix of multiple risk factors may be applied.

The limitations of mean Value-at-Risk are well covered in the literature. The limitations of traditional mean-VaR are all related to the use of a symetrical distribution function.  Use of simulations, resampling, or Pareto distributions all help in making a more accurate prediction, but they are still flawed for assets with significantly non-normal (skewed or kurtotic) distributions. Zangari (1996) and Favre and Galeano(2002) provide a modified VaR calculation that takes the higher moments of non-normal distributions (skewness, kurtosis) into account through the use of a Cornish Fisher expansion, and collapses to standard (traditional) mean-VaR if the return stream follows a standard distribution. This measure is now widely cited and used in the literature, and is usually referred to as \dQuote{Modified VaR} or \dQuote{Modified Cornish-Fisher VaR}.They arrive at their modified VaR calculation in the following manner:

\deqn{z_{cf}=q_{c}+\frac{(q_{c}^{2}-1)S}{6}+\frac{(q_{c}^{3}-3q_{c})K}{24}-\frac{(2q_{c}^{3}-5q_{c})S^{2}}{36}}

\deqn{modVaR =\bar{R} - \sigma \cdot z_{cf}}

where $S$ is the skewness of $R$ and $K$ is the excess kurtosis of $R$.

Cornish-Fisher VaR collapses to traditional mean-VaR when returns are normally distributed. As such, the \code{\link{VaR.mean}} and \code{\link{VaR.traditional}} functions are wrappers for the \code{VaR.CornishFisher} function. The Cornish-Fisher expansion also naturally encompasses much of the variability in returns that could be uncovered by more computationally intensive techniques such as resampling or Monte-Carlo simulation.  This is the default method for the \code{VaR} function, and may be accessed by setting \code{method="modified"}.

Favre and Galeano also utilize modified VaR in a modified Sharpe Ratio as the return/risk measure for their portfolio optimization analysis, see \code{\link{SharpeRatio.modified}} for more information.

Epperlein and Smillie (2006) introduced a non-parametric kernel estimator for component risk contributions, which is available via \code{method="kernel"} and \code{portfolio_method="component"}.
}
\section{Component VaR}{
By setting \code{portfolio_method="component"} you may calculate the risk contribution of each element of the portfolio.  The return from the function in this case will be a list with three components: the univariate portfolio VaR, the scalar contribution of each component to the portfolio VaR (these will sum to the portfolio VaR), and a percentage risk contribution (which will sum to 100\%). 

Both the numerical and percentage component contributions to VaR may contain both positive and negative contributions.  A negative contribution to Component VaR indicates a portfolio risk diversifier.  Increasing the position weight will reduce overall portoflio VaR.

If a weighting vector is not passed in via \code{weights}, the function will assume an equal weighted (neutral) portfolio.  

\emph{INSERT MORE DETAILS AND REFERENCES HERE, include details on passing moments into function for greater efficiency or improved estimators.}
}
\section{Marginal VaR}{
Different papers call this different things.  In the Denton and Jayaraman paper referenced here, this calculation is called Incremental VaR.  We have chosen the more common usage of calling this difference in VaR's in portfolios without the instrument and with the instrument as the \dQuote{difference at the Margin}, thus the name Marginal VaR.  This is incredibly confusing, and hasn't been resolved in the literature at this time.  Simon Keel and David Ardia (2009) attempt to reconcile some of the definitional issues and address some of the shortcomings of this measure in their working paper titled \dQuote{Generalized Marginal Risk}.  Hopefully their improved Marginal Risk measures may be included here in the future. 
}
}
\value{
VaR measure
}
\references{
Zangari, Peter. A VaR Methodology for Portfolios that include Options. 1996. RiskMetrics Monitor, First Quarter, 4-12.

Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.

Uryasev S, Rockafellar R. Optimization of Conditional VaR. University of Florida, Working paper, 1999.

Epperlein, E., Smillie, A. Cracking VaR with kernels. 2006. RISK 19 (8), 70–74.

Denton M. and Jayaraman, J.D. Incremental, Marginal, and Component VaR. Sunguard. 2004.


}
\author{ Brian G. Peterson and Kris Boudt }
\note{
The option to \code{invert} the VaR measure should appease both academics and practitioners.  The mathematical definition of VaR as the negative value of a quantile will (usually) produce a positive number.  Practitioners will argue that VaR denotes a loss, and should be internally consistent with the quantile (a negative number).  For tables and charts, different preferences may apply for clarity and compactness.  As such, we provide the option, and set the default to TRUE to keep the return consistent with prior versions of PerformanceAnalytics, but make no value judgement on which approach is preferable.

The prototype of the univariate Cornish Fisher VaR function was completed by Prof. Diethelm Wuertz.  All corrections to the calculation and error handling are the fault of Brian Peterson.
}
\seealso{
    \code{\link{SharpeRatio.modified}} \cr
    \code{\link{chart.VaRSensitivity}} \cr
    \code{\link[VaR]{VaR.gpd}} \cr
    \code{\link[VaR]{VaR.norm}} \cr
    \code{\link[VaR]{VaR.backtest}} \cr
}
\examples{
    data(edhec)

    # first do normal VaR calc
    VaR(edhec, p=.95, method="historical")

    # now use modified Cornish Fisher calc to take non-normal distribution into account
    VaR(edhec, p=.95, method="modified")

    # now use default p=.99
    VaR(edhec)

    # now with outliers squished
    VaR(edhec, clean="boudt")

    # add Component VaR for the equal weighted portfolio
    VaR(edhec, clean="boudt", portfolio_method="component")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }