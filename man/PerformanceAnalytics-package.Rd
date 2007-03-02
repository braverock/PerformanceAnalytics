\name{PerformanceAnalytics-package}
\alias{PerformanceAnalytics-package}
\alias{PerformanceAnalytics}
\docType{package}
\title{
Econometric tools for performance and risk analysis.
}
\description{
Library of econometric functions for performance and risk analysis of financial portfolios. This library aims to aid practitioners and researchers in using the latest research in analysis of both normal and non-normal return streams.  In general, this library is most tested on return (rather than price) data on a monthly scale. Many functions will work with daily or irregular return data as well.  See function \code{\link{CalculateReturns} for calculating returns from prices, and be aware that the \code{\link{aggregate}} function has methods for \code{tseries} and \code{zoo} classes to rationally coerce irregular data into regular data of the correct periodicity.
}
\details{
\tabular{ll}{
Package: \tab PerformanceAnalytics\cr
Type: \tab Package\cr
Version: \tab 0.9.2\cr
Date: \tab 2007-03-01\cr
License: \tab GPL\cr
}

We created this library to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in R.  In doing so, we also found it valuable to have wrapper functions for functionality easily replicated in R, so that we could access that functionality using a function with defaults and naming consistent with common usage in the finance literature.  The following sections on Performance Analysis, Risk Analysis (with a separate treatment of VaR), Summary Tables of related statistics, Charts and Graphs, a vaiety of Utility Functions, and some thoughts on work yet to be done.
}

\section{Performance Analysis}{
Distribution analysis
tests for normal
variance appears below under Risk
skewness
kurtosis
co-moments
\code{\link{ActivePremium}} \code{\link{InformationRatio}} \code{\link{TrackingError}}
\code{\link{KellyRatio}}

systematic moments as indicators of diversification potential
\code{\link{CAPM.alpha}}
\code{\link{CAPM.beta}}

rolling period analysis


}

\section{Risk Analysis}{

Many methods have been proposed to measure, monitor, and control the risks of a diversified portfolio. Perhaps a few definitions are in order on how different risks are generally classified. \emph{Market Risk} is the risk to the portfolio from a decline in the market price of instruments in the portfolio.  \emph{Liquidity Risk} is the risk that the holder of an instrument will find that a position is illiquid, and will incur extra costs in unwinding the position resulting in a less favorable price for the instrument. In  extreme cases of liquidity risk, the seller may be unable to find a buyer for the instrument at all, making the value unknowable or zero.  \emph{Credit Risk} is also sometimes called \emph{Default Risk}, or the risk that promised payments on a loan or bond will not be made, or that a convertible instrument will not be converted in a timely manner or at all.  There are also additional \emph{Counterparty Risks} in manual or over the counter markets, and in many complex derivatives.  Tools have evolved to measure all these different components of risk.  Processes must be put into place inside a firm to monitor the changing risks in a portfolio, and to control the magnitude of risks.  For an extensive treatment of these topics, see Litterman, Gumerlock, et. al. (1998).

The simplest risk measure in common use is volatility, usually modeled quantitatively with a univariate standard deviation on a portfolio.  See \code{\link{StdDev}}.  Volatility or Standard Deviation is an appropriate risk measure when the distribution of returns is normal or resembles a random walk, and may be annualized using \code{\link{StdDev.annualized}}.  Many assets, including hedge funds, commodities, options, and even most common stocks over a sufficiently long period, do not follow a normal distribution.  For such common but non-normally distributed assets, a more sophisticated approach than standard deviation/volatility is required to adequately model the risk.

Markowitz, in his Nobel acceptance speech and in several papers, proposed that \code{\link{SemiVariance}} would be a better measure of risk than variance.  See Zin,Markowitz,Zhao, 2006 \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=910640}.  This measure is also called \code{\link{SemiDeviation}}.  The more general case of downside deviation is implemented in the function \code{\link{DownsideDeviation}}, as proposed by Sortino and Price (1994) where the minimum acceptable return (MAR) is a parameter to the function.  It is interesting to note that variance and mean return can produce a smothly eliptical efficient frontier for portfolio optimization utilizing \code{\link{solve.QP}} or \code{\link{portfolio.optim}} or \code{\link{MarkowitzPortfolio}}. Use of semivariance or many other risk measures will not necessarily create a smooth elipse, causing significant additional difficulties for the portfolio manager trying to build an optimal portfolio.  We'll leave a more complete treatment and implementation of portfolio optimization techniques for another time.

Another very widely used downside risk measures is analysis of drawdowns, or loss from peak value achieved. The simplest method is to check the \code{\link{MaxDrawdown}}, as this will tell you the worst cumulative loss ever sustained by the asset.  If you want to look at all the drawdowns, you can \code{\link{findDrawdowns}} and \code{\link{sortDrawdowns}} in order from worst/major to smallest/minor. The \code{\link{UpDownRatios}} function will give you some insight into skew and kurtosis of the returns, by letting you know how length and magnitude of up or down moves compare to each other.  You can also plot drawdowns with \code{\link{chart.Drawdowns}}.

One of the most commonly used and cited measures of the risk/reward tradeoff of an investment or portfolio is the \code{\link{SharpeRatio}}, which measures return over standard deviation.  If you are comparing multiple assets using Sharpe, you should use \code{\link{SharpeRatio.annualized}}. The \code{\link{SortinoRatio}} utlizes mean return over \code{\link{DownsideDeviation}} below the MAR as the risk measure to produce a similar ratio that is more sensitive to downside risk.  Favre and Galeano (2002) propose utilizing the ratio of mean return over the Cornish Fisher \code{\link{modifiedVaR}} to produce \code{\link{SharpeRatio.modified}}. \code{\link{TreynorRatio}} is also similar to the Sharpe Ratio, except it uses \code{\link{CAPM.beta}} in place of the volatility measure to produce the ratio of the investment's excess return over the beta.

One of the newest statistical methods developed for analyzing the risk of financial instruments is \code{\link{Omega}}.  Omega analytically constructs a cumulative distribution function, in a manner similar to \code{\link{chart.QQplot}}, but then extracts additional information from the location and slope of the derived function at the point indicated by the risk quantile that the researher is interested in.  Omega seeks to combine a large amount of data about the shape, magnitude, and slope of the distribution into one method.  Omega is still a very new method, and the academic literature is still exploring the best methods to utilize it in a risk measurement and control process, or in portfolio construction.

Any risk measure should be viewed with suspicion if there are not a large number of historical observations of returns for the asset in question available.  Depending on the measure, how many observations are required will vary greatly from a statistical standpoint.  As a heuristic rule, ideally you will have data available on how the instrument performed through several economic cycles and shocks.  When such a long history is not available, the investor or researcher has several options.  A full discussion of the various approaches is beyond the scope of this introduction, so we will merely touch on several areas that an interested party may wish to explore in additional detail. Examining the returns of assets with a similar style, industry, or asset class to which the asset in question is highly correlated and shares other characteristics can be quite informatinve.  Factor analysis may be utilized to uncover specific risk factors where transparency is not available. Various resampling and simulation methods are available to construct an artificially long distribution for testing.
}

\section{Value at Risk - VaR}{
In the early 90's, academic literature started referring to "value at risk", typically written as VaR. Take care to capitalize VaR in the commonly accepted manner, to avoid confusion with var (variance) and VAR (vector auto-regression).  With a sufficiently large data set, you may choose to utilize a non-parametric VaR estimation method using the historical distribution and the probability quantile of the distribution calculated using \code{\link{qnorm}}. The negative return at the correct quantile (usually 95\% or 99\%), is the non-parametric VaR estimate.  J.P. Morgan's RiskMetrics parametric mean-VaR was published in 1994 and this methodology for estimating parametric mean-VaR has become what people are generally referring to as "VaR" and what we have implemented as \code{\link{VaR.traditional}}.  See "Return to RiskMetrics: Evolution of a Standard"\url{http://www.riskmetrics.com/r2rovv.html}, \code{fPortfolio} has also implemented traditional mean-VaR as the \code{\link{VaR}} function.  Paremetric traditional VaR does a better job of accounting for the tails of the distribution by more heavily weighting the tails below the risk quantile.  It is still insufficient if the assets have a distribution that varys widely from normality.

The R package \code{VaR} contains methods for simulating and estimating lognormal \code{\link{VaR.norm}} and generalized Pareto \code{\link{VaR.gpd}} distributions to overcome some of the problems with nonparametric or parametric mean-VaR calculations on a limited sample size.  There is also a \code{\link{VaR.backtest}} function to apply simulation methods to create a more robust estimate of the potential distribution of losses.  The VaR package also provides plots for its functions.

The \code{fPortfolio} package has implemented Conditional Value at Risk, also called Expected Shortfall, in functions \code{\link{CVaR}} and \code{\link{CVaRplus}}.  Expected Shortfall attempts to measure the magnitude of the average loss exceeding the traditional mean-VaR. Expected Shortfall has proven to be a reasonable risk predictor for many asset classes.  See Uryasev (2000) for more information on Conditional Value at Risk.  A similar measure called Beyond VaR implemented here as \code{\link{VaR.Beyond}} also attempts to estimate average loss beyond traditional mean-VaR.  Adding mean-VaR and Beyond VaR will produce a measure very similar to \code{\link{CVaRplus}}. Please note that your milage will vary; expect that values obtained from the normal distribution may differ radically from the real situation, depending on the assets under analysis.

Cornish-Fisher VaR

Incremental and Component VaR
Marginal VaR

Which measure to use will depend greatly on the portfolio and instruments being analyzed.  If there is any generalization to be made on VaR measures, this author will agree with Bali and Gokcan (2004) who conclude that "the VaR estimations based on the generalized Pareto distribution and the Cornish-Fisher approximation perform best".
}

\section{Summary Tabular Data}{

}

\section{Charts and Graphs}{

}

\section{Wrapper and Utility Functions}{
R is a very powerful envoronment for manipulating data.  It can also be quite confusing to a user more accustomed to Excel or even MatLAB.  As such, we have written some wrapper functions that may aid you in coercing data into the correct forms or finding data that you need to use regularly.  To simplify the management of multiple-source data stored in R in multiple data formats, we have provided \code{\link{checkDataMatrix}} and \code{\link{checkDataVector}}.  These functions will attempt to coerce data in an of R's multitude of mostly funcible data classes into the class required for a particular analysis.  In general, the use of these data-coercion functions has been hidden inside the business functions provided.  They may also save you time and trouble in your own code and functions outside of the functionality provided by the PerformanceAnalytics libary.

When you are anlyzing relative or absolute performance of investments, you need to analyse returns, but much data is available only as prices.  We have provided the simple wrapper function \code{\link{CalculateReturns}} to address this by taking a stream of prices and calculating simple or compounded returns from the price vector. The excellent tseries class that is part of the R core includes the function \code{\link{get.hist.quote}} for retrieving market data from online sources.  Many of the functions in PerformanceAnalytics require either a benchmark or a risk free rate, and many practitioners will not have access to a formal database of returns.  To facilitate the examples and provide an example of how to retrieve and coerce the data, we have provided functions for S&P 500 returns in the \code{\link{download.SP500PriceReturns}} and the 13-day US Treasury Bill in \code{\link{download.RiskFree}}.

\code{\link{cummax.column}}

\code{\link{cumprod.column}}

\code{\link{legend}}

}

\section{Further Work}{
We have attempted to standardize function parameters and variable names, but more work exists to be done here.
In some cases, there are separate functions for single column and multicolumn data, which could be combined into a single function for ease of use and clarity.
There are functions included in this package that would benefit from more complex examples.
There is also an opportunity to extend several of the co-moments and risk measures to use multivariate estimates of the higher moments, which should produce more robust estimates of the moments of a portfolio of multiple diverse assets.
Optimization
Factor Analysis
Hedge Selection and Analysis
Shock/Scenario Analysis

Suggestions Welcome

If you've implemented anything in the list above, please consider donating it for inclusion in a later version of this package.
}

\author{
Peter Carl \cr
Brian G. Peterson \cr

Maintainer: Brian G. Peterson <brian@braverock.com> \url{http://braverock.com/brian/}
}

\references{
Lhabitant F. \emph{Hedge Funds: Quantitative Insights}. Wiley. 2004. \cr
Litterman R., Gumerlock R., et. al. \emph{The Practice of Risk Management: Implementing Processes for Managing Firm-Wide Market Risk}. Euromoney. 1998. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
Tsay, R. \emph{Analysis of Financial Time Series}. Wiley. 2001. \cr
Zivot, E. and Wang, Z. \emph{Modeling Financial Time Series with S-Plus: second edition}. Springer. 2006. \cr
}

\keyword{ package }
\seealso{}
\examples{
}