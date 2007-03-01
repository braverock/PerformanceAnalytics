\name{PerformanceAnalytics-package}
\alias{PerformanceAnalytics-package}
\alias{PerformanceAnalytics}
\docType{package}
\title{
Econometric tools for performance and risk analysis.
}
\description{
Library of econometric functions for performance and risk analysis of financial portfolios. This library aims to aid practitioners and researchers in using the latest research in analysis of both normal and non-normal return streams.  In general, this library is most tested on return (rather than price) data on a monthly scale, but most functions will work with daily or irregular return data as well.
}
\details{
\tabular{ll}{
Package: \tab PerformanceAnalytics\cr
Type: \tab Package\cr
Version: \tab 0.9.2\cr
Date: \tab 2007-03-01\cr
License: \tab GPL\cr
}
We created this library to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in R.  In doing so, we also found it valuable to have wrapper functions for functionality easily replicated in R, so that we could access that functionality using a function with defaults and naming consistent with common usage in the finance literature.

\emph{Performance Analysis}

\emph{Risk Analysis}

Many methods have been proposed to monitor the risk of a diversified portfolio.  The simplest risk measure in common use is volatility, usually modeled quantitatively with a univariate standard deviation on a portfolio.  See \code{\link{StdDev}}.  Volatility or Standard Deviation is an appropriate measure when the distribution of returns is normal or resembles a random walk.  Many assets, including hedge funds, commodities, options, and even most common stocks over a sufficiently long period, do not follow a normal distribution.  For such common but non-normally distributed assets, a more sophisticated approach than standard deviation/volatility is required.

Markowitz, in his Nobel acceptance speach and in several papers, proposed that \code{\link{SemiVariance}} would be a better measure of risk than variance.  See Zin,Markowitz,Zhao, 2006 \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=910640}.  This measure is also called \code{\link{SemiDeviation}}.  It is interesting to note that variance and mean return can produce a smothly eliptical efficient frontier for portfolio optimization utilizing \code{link{solve.QP}} or \code{\link{portfolio.optim}} or \code{\link{MarkowitzPortfolio}}. Use of semivariance will not necessarily create a smooth elipse, causing significant additional difficulties for the portfolio manager trying to build an optimal portfolio.  The more general case of downside deviation is implemented in the function \code{\link{DownsideDeviation}}, as proposed by Sortino and Price (1994) where the minimum acceptable return (MAR) is a parameter to the function.

Talk about the various VaR measures here...

One of the most common measures of the risk/reward tradeoff of an investment or portfolio is the \code{\link{SharpeRatio}}, which measures return over standard deviation.  If you are comparing multiple assets using Sharpe, you should use \code{\link{SharpeRatio.annualized}}. The \code{\link{SortinoRatio}} utlizes mean return over \code{\link{DownsideDeviation}} below the MAR as the risk measure to produce a similar ratio that is more sensitive to downside risk.  Favre and Galeano (2002) propose utilizing the the ratio of mean return over the Cornish Fisher \code{\link{modifiedVaR}} to produce \code{\link{SharpeRatio.modified}}.

\emph{Summary Tabular Data}

\emph{Charts and Graphs}

\emph{Wrapper and Utility Functions}
R is a very powerful envoronment for manipulating data.  It can also be quite confusing to a user more accustomed to Excel or even MatLAB.  As such, we have written some wrapper functions that may aid you in coercing data into te correct forms or finding data that you need to use regularly.  To simplify the management of multiple-source data stored in R in multiple data formats, we have provided \code{\link{CheckDataMatrix}} and \code{\link{CheckDataVector}}.  These functions will attempt to coerce data in an of R's multitude of mostly funcible data classes into the class required for a particular analysis.  In general, the use of these data-coercion functions has been hidden inside the business functions provided.  They may also save you time and trouble in your own code and functions outside of the functionality provided by the PerformanceAnalytics libary.

When you are anlyzing relative or absolute performance of investments, you need to analyse returns, but much data is available only as prices.  We have provided the simple wrapper function \code{\link{CalculateReturns}} to address this by taking a stream of prices and calculating simple or compounded returns from the price vector. The excellent tseries class that is part of the R core includes the function \code{\link{get.hist.quote}},  Many of the functions in PerformanceAnalytics require either a benchmark or a risk free rate, and many practitioners will not have access to a formal database of returns.  To facilitate the examples and provide an example of how to retrieve and coerce the data, we have provided functions for S&P 500 returns in the \code{\link{download.SP500PriceReturns}} and the 13-day US Treasury Bill in \code{\link{download.RiskFree}}.

}
\author{
Peter Carl \cr
Brian G. Peterson \cr

Maintainer: Brian G. Peterson <brian@braverock.com> \url{http://braverock.com/brian/}
}

\references{
Lhabitant F. \emph{Hedge Funds: Quantitative Insights}. Wiley. 2004. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
Tsay, R. \emph{Analysis of Financial Time Series}. Wiley. 2001. \cr
Zivot, E. and Wang, Z. \emph{Modeling Financial Time Series with S-Plus: second edition}. Springer. 2006. \cr
}

\keyword{ package }
\seealso{}
\examples{
}
