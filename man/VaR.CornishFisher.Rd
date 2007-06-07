\name{VaR.CornishFisher}
\alias{VaR.CornishFisher}
\alias{VaR.traditional}
\alias{VaR.mean}
\alias{VaR}
\alias{modifiedVaR}
\title{ calculate various Value at Risk (VaR) measures }
\description{
These functions calculate both traditional mean-VaR and modified Cornish-Fisher VaR
}
\usage{
VaR.CornishFisher(R, p = 0.99, modified = TRUE)
modifiedVaR(R, p = 0.99)
VaR.traditional(R, p = 0.99)
VaR.mean(R, p = 0.99)
VaR(R, p = 0.99)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{modified}{ TRUE/FALSE default TRUE, use Cornish Fisher Expansion to take higher moments into account }
}
\details{
In the early 90's, academic literature started referring to \dQuote{value at risk}, typically written as VaR. Take care to capitalize VaR in the commonly accepted manner, to avoid confusion with var (variance) and VAR (vector auto-regression).  With a sufficiently large data set, you may choose to utilize a non-parametric VaR estimation method using the historical distribution and the probability quantile of the distribution calculated using \code{\link{quantile}} for an arbitrary historical distribution or \code{\link{qnorm}} for an idealized normal distribution. The negative return at the correct quantile (usually 95\% or 99\%), is the non-parametric historical VaR estimate.

In a set of returns for which sufficently long history exists, the per-period Value at risk is simply the correct quantile of the period losses \eqn{\mu}  with variance \eqn{\sigma}  is denoted by:

\deqn{VaR=\mu_{q_{.99}}}
where \eqn{q_{.99}} is the 99\% quantile of the distribution.

This method is also sometimes called \dQuote{historical VaR}, as it is by definition \emph{ex post} analysis of the return distribution.

When you don't have a sufficiently long set of returns to use non-parametric or historical VaR, or wish to more closely model an ideal distribution, it is common to us a parmetric estimate based on the distribution. J.P. Morgan's RiskMetrics parametric mean-VaR was published in 1994 and this methodology for estimating parametric mean-VaR has become what most literature generally refers to as \dQuote{VaR} and what we have implemented as \code{\link{VaR.traditional}}. See \cite{Return to RiskMetrics: Evolution of a Standard}\url{http://www.riskmetrics.com/r2rovv.html},

Parametric mean-VaR does a better job of accounting for the tails of the distribution by more precisely estimating shape of the distribution tails of the risk quantile. The most common estimate is a normally distributed \eqn{R\sim N(\mu,\sigma^{2})} return series. In this case, estimation of VaR requires the mean return \eqn{\bar{R}}, the distribution of losses \eqn{\mu}, and the variance of the returns \eqn{\sigma}. In the most common case, parametric VaR is thus calculated by

\deqn{\sigma=variance(R)}

\deqn{VaR=\bar{R}+\sigma\cdot q_{.99}}

where \eqn{q_{.99}} is the 99\% quantile of the distribution. Represented in \R by \code{qnorm(1-p)}

\code{fPortfolio} has also implemented traditional mean-VaR as the \code{\link[fPortfolio]{VaR}} function.

Other forms of parametric mean-VaR estimation utilize a different distribution for the distribution of losses \eqn{\mu} to better account for the possible fat-tailed nature of downside risk. The package \code{VaR} contains methods for simulating and estimating lognormal \code{\link[VaR]{VaR.norm}} and generalized Pareto \code{\link[VaR]{VaR.gpd}} distributions to overcome some of the problems with nonparametric or parametric mean-VaR calculations on a limited sample size or on potentially fat-tailed distributions.  There is also a \code{\link[VaR]{VaR.backtest}} function to apply simulation methods to create a more robust estimate of the potential distribution of losses. Less commonly a covariance matrix of multiple risk factors may be applied.

The limitations of mean Value-at-Risk are well covered in the literature. The limitations of traditional mean-VaR are all related to the use of a symetrical distribution function.  Use of simulations, resampling, or Pareto distributions all help in making a more accurate prediction, but they are still flawed for assets with significantly non-normal (skewed or kurtotic) distributions. Favre and Galeano(2002) provide a modified VaR calculation that takes the higher moments of non-normal distributions (skewness, kurtosis) into account through the use of a Cornish Fisher expansion, and collapses to standard (traditional) mean-VaR if the return stream follows a standard distribution. This measure is now widely cited and used in the literature, and is usually referred to as \dQuote{Modified VaR} or \dQuote{Modified Cornish-Fisher VaR}.They arrive at their modified VaR calculation in the following manner:

\deqn{z_{c} = q_{p}=qnorm(p)}

\deqn{S= skewness(R)}

\deqn{K = kurtosis(R)}

\deqn{z_{cf}=z_{c}+\frac{(z_{c}^{2}-1)S}{6}+\frac{(z_{c}^{3}-3z_{c})K}{24}+\frac{(2z_{c}^{3}-5z_{c})S^{2}}{36}}

\deqn{modVaR =\bar{R}- z_{cf}\sqrt{\sigma}}

Cornish-Fisher VaR collapses to traditional mean-VaR when returns are normally distributed. As such, the \code{\link{VaR.mean}} and \code{\link{VaR.traditional}} functions are wrappers for the \code{VaR.CornishFisher} function. The Cornish-Fisher expansion also naturally encompasses much of the variability in returns that could be uncovered by more computationally intensive techniques such as resampling or Monte-Carlo simulation.

Favre and Galeano also utilize modified VaR in a modified Sharpe Ratio as the return/risk measure for their portfolio optimization analysis, see \code{\link{SharpeRatio.modified}} for more information.
}
\value{
VaR measure
}
\references{
Laurent Favre and Jose-Antonio Galeano. Mean-Modified Value-at-Risk Optimization with Hedge Funds. Journal of Alternative Investment, Fall 2002, v 5.

Uryasev S, Rockafellar R. Optimization of Conditional VaR. University of Florida, Working paper, 1999.
}
\author{ Brian G. Peterson }
\note{
The prototype of the Cornish Fisher VaR function was completed by Diethelm Wuertz.  All corrections to the calculation and error handling ar the fault of Brian Peterson.
}
\seealso{
    \code{\link{VaR.Beyond}}  \cr
    \code{\link{VaR.Marginal}} \cr
    \code{\link{SharpeRatio.modified}} \cr
    \code{\link[fBasics]{skewness}} \cr
    \code{\link[fBasics]{kurtosis}} \cr
    \code{\link[fPortfolio]{VaR}} \cr
    \code{\link[VaR]{VaR.gpd}} \cr
    \code{\link[VaR]{VaR.norm}} \cr
    \code{\link[VaR]{VaR.backtest}} \cr
    \code{\link[fPortfolio]{CVaR}}
}
\examples{
    data(edhec)

    # first do normal VaR calc
    VaR.traditional(edhec, p=.95)

    # now use modified Cornish Fisher calc to take non-normal distribution into account
    VaR.CornishFisher(edhec, p=.95)

    # now use default p=.99
    VaR.CornishFisher(edhec)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }