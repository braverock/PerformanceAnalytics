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

We created this library to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in R.  In doing so, we also found it valuable to have wrapper functions for functionality easily replicated in R, so that we could access it with defaults and naming consistent with common usage in the finance literature.  To introduce the capabilities of this package, we group the functions into 
We created this library to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in R.  In doing so, we also found it valuable to have wrapper functions for functionality easily replicated in R, so that we could access that functionality using a function with defaults and naming consistent with common usage in the finance literature.  The following sections on Performance Analysis, Risk Analysis (with a separate treatment of VaR), Summary Tables of related statistics, Charts and Graphs, a vaiety of Utility Functions, and some thoughts on work yet to be done.
}

\section{Performance Analysis}{

How did that fund do last year? If it made 15\%, is that good?  To most people, performance seems like a simple subject with clear absolutes: did it make money?  But scratch the surface a bit and nagging questions appear: Am I paying too much for what I'm getting?  What risks am I taking? Does it fit in my portfolio?  Do I have too much?  Too little?  The answer to the initial question, we find out, depends.  It depends on the substitutes available, the risks taken, the liquidity of the vehicle, the potential for ruin, the rest of the portfolio.  

The literature around the subject of performance analysis seems to have exploded with the popularity of the hedge fund.  Simpler tools that seemed appropriate in a relative investment world seem inappropriate for an absolute return world.  

Performance measurement is the first step down a longer road.  Risk measurement, which is nearly inseparable from performance assessment, has been on a long march to become multi-dimensional while remaining conceptually accessible (How much could I lose?).  Portfolio construction and risk budgeting are two sides of the same coin (How do I maximize my expected gain\avoid going broke?).  But first we have to answer the question: Is this something I might want in my portfolio?

With the rise of the investor class during the last decades, and the increasing availability of complicated investment strategies to retail investors, and the broad availability of financial data, an engaging debate about performance analysis and evaluation is as important as ever.  There won't be one \emph{right} answer, a silver bullet, delivered in these metrics and charts.  What there will be is an accretion of evidence, organized to \emph{assist} a decision maker in answering a specific question that is pertinant to the decision at hand.  Using such tools to uncover information and ask better questions will, in turn, create a more informed investor.

Performance measurement starts with returns.  Proprietary traders will object, complaining that \"You can't eat returns,\" and will prefer to look at numbers with currency signs.  To some extent, they have a point - the normalization inherent in calculating returns can be deceiving.  Most of the recent work in financial econometrics, however, is focused on returns rather than prices.  This "price per unit of investment" standardization is important for two reasons - it helps the decision maker to compare opportunities, and it has some useful statistical qualities.  As a result, this module will focus on returns.  See \code{\link{CreateReturns}} for converting net asset values or prices into returns, either discrete or continuous. 

Returns and risk can then be annualized as a way to simplify comparison over longer time periods.  Although it requires a bit of estimating, such aggregation is popular because it offers a reference point for easy comparison.  Examples are in \code{\link{Return.annualized}}, \code{\link{StdDev.annualized}}, and \code{\link{SharpeRatio.annualized}}. 

\section{Summary Tabular Data}{
Return statistics are then the necessary aggregation and reduction of these (potentially thousands) of periodic numbers. Usually these are most palatable when organized into a table of related statistics, assembled for a particular purpose.  A common offering of past returns organized by month and cumulated by calendar year is usually presented as a table, such as in \code{\link{table.Returns}}.  Adding benchmarks or peers alongside the annualized data is helpful for comparing returns in calendar years.

A number of other tables constructed for comparison are included.  When we started this project, we debated whether or not such tables would be broadly useful or not.  No reader is likely to think that we captured the necessary statistics to help their decision, so we merely offer these as a starting point for creating your own.  Add, subtract, do whatever seems useful to you.  We have aggregated the statistics touched on above into these tables that we have found useful so far: 
\itemize{\code{\link{table.MonthlyReturns}}, which provides a statistical summary of the monthly returns themselves;}
\itemize{\code{\link{table.AnnualizedReturns}}, which calculates aggregate returns and risk (usually used in graphics discussed below); }
\itemize{\code{\link{table.CAPM}}, a collection of statistics offered by practitioners of Modern Portfolio Theory; }
\itemize{\code{\link{table.HigherMoments}}, a table of metrics for comparing higher moments of distributions, including the four-moment CAPM;  }
\itemize{\code{\link{table.DownsideRisk}}, statistics for those wanting to compare the extent of observed losses; }
\itemize{\code{\link{table.Correlation}}, a table arranged for comparing correlations; and } 
\itemize{\code{\link{table.RollingPeriods}}, which calculates statistics over varying periods of time.  }
}

\section{Charts and Graphs}{
Alternatively, graphs and charts can help to organize the information visually, again for a particular purpose.  The cumulative returns or wealth index is usually the first thing displayed, even though they rarely convey much information.  See \code{\link{chart.CumReturns}}.  Individual period returns may be helpful for identifying problematic periods, such as in \code{\link{chart.Bar}}.  Risk measures can be helpful when overlayed on the period returns, to display the bounds at which losses may be expected.  See \code{\link{chart.BarVaR}} and the following section.  More information can be conveyed when such charts are displayed together, as in \code{\link{chart.PerformanceSummary}}, which pairs the performance data with detail on downside risk (see \code{\link{chart.Drawdown}}).  

Two-dimensional charts can also be useful while remaining easy to understand.  \code{\link{chart.Scatter}} is a utility scatter chart with some additional attributes that are used in \code{\link{chart.RiskReturnScatter}}.  Overlaying Sharpe ratio lines or boxplots helps to add information about relative performance along those dimensions.  Of course, 

Rolling performance is typically used as a way to assess stability of a return stream.  Although perhaps not stati

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
<<<<<<< PerformanceAnalytics-package.Rd
There is also an opportunity to extend several of the co-moments and risk measures to use multivariate estimates of the higher moments, which should produce more robust estimates of the moments of a portfolio of multiple diverse assets.  
Any comments or suggestions to this end are very welcome.
=======
There is also an opportunity to extend several of the co-moments and risk measures to use multivariate estimates of the higher moments, which should produce more robust estimates of the moments of a portfolio of multiple diverse assets.
Optimization
Factor Analysis
Hedge Selection and Analysis
Shock/Scenario Analysis

Suggestions Welcome

If you've implemented anything in the list above, please consider donating it for inclusion in a later version of this package.
>>>>>>> 1.18
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
