\name{PerformanceAnalytics-package}
\alias{PerformanceAnalytics-package}
\alias{PerformanceAnalytics}
\docType{package}
\title{
Econometric tools for performance and risk analysis.
}

\description{
Library of econometric functions for performance and risk analysis of financial portfolios. This library aims to aid practitioners and researchers in using the latest research in analysis of both normal and non-normal return streams.

We created this library to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in \R.  In doing so, we also found it valuable to have wrapper functions for functionality easily replicated in \R, so that we could access that functionality using a function with defaults and naming consistent with common usage in the finance literature.  The following sections cover Performance Analysis, Risk Analysis (with a separate treatment of VaR), Summary Tables of related statistics, Charts and Graphs, a variety of Wrappers and Utility functions, and some thoughts on work yet to be done.
}

\details{
    \tabular{rl}{
        Package: \tab PerformanceAnalytics\cr
        Type: \tab Package\cr
        Version: \tab 0.9.7.1\cr
        Date: \tab 2008-10-10\cr
        License: \tab GPL\cr
        Copyright: \tab (c) 2004-2008 Brian G. Peterson and Peter Carl\cr
        URL: \tab \url{http://braverock.com/R/} \cr
        URL: \tab \url{http://cran.r-project.org/web/packages/PerformanceAnalytics/index.html}
    }

In general, this library is most tested on return (rather than price) data on a monthly scale. Many functions will work with daily or irregular return data as well.  See function \code{\link{CalculateReturns}} for calculating returns from prices, and be aware that the \code{\link{aggregate}} function has methods for \code{tseries} and \code{zoo} timeseries data classes to rationally coerce irregular data into regular data of the correct periodicity.

In this summary, we attempt to provide an overview of the capabilities provided by \code{PerformanceAnalytics} and pointers to other literature and resources in \R needed for performance and risk analysis.  We hope that this summary and the accompanying package and documentation partially fill a hole in the tools available to a financial engineer or analyst.
}

\section{Performance Analysis}{
Performance measurement is the first step down a longer road.  When discussing performance at an abstract level, terms such as \dQuote{return} and \dQuote{risk} are deliberately vague because they help to disguise some (usually unnecessary) complexity.  Scratch the surface to answer more complicated questions, and that complexity is necessarily uncovered again.  Indeed, the literature around the subject of performance analysis seems to have exploded with the popularity of alternative assets such as hedge funds, managed futures, commodities, and structured products. Simpler tools that seemed appropriate in a relative investment world seem inappropriate for an absolute return world.  Risk measurement, which is nearly inseparable from performance assessment, has been on a long march to become multi-dimensional and multi-moment while trying to answer the question: \dQuote{How much could I lose?}  Portfolio construction and risk budgeting are two sides of the same coin: \dQuote{How do I maximize my expected gain and avoid going broke?}  But before we can approach those questions we first have to ask: \dQuote{Is this something I might want in my portfolio?}

With the the increasing availability of complicated alternative investment strategies to both retail and institutional investors, and the broad availability of financial data, an engaging debate about performance analysis and evaluation is as important as ever.  There won't be one \emph{right} answer delivered in these metrics and charts.  What there will be is an accretion of evidence, organized to \emph{assist} a decision maker in answering a specific question that is pertinent to the decision at hand.  Using such tools to uncover information and ask better questions will, in turn, create a more informed investor.

Performance measurement starts with returns.  Proprietary traders will object, complaining that \dQuote{You can't eat returns,} and will prefer to look at numbers with currency signs.  To some extent, they have a point - the normalization inherent in calculating returns can be deceiving.  Most of the recent work in financial econometrics, however, is focused on returns rather than prices.  This \dQuote{price per unit of investment} standardization is important for two reasons - first, it helps the decision maker to compare opportunities, and second, it has some useful statistical qualities.  As a result, the \code{PerformanceAnalytics} library focuses on returns.  See \code{\link{CalculateReturns}} for converting net asset values or prices into returns, either discrete or continuous.  Many papers and theories refer to \dQuote{excess returns}: we have implemented the most common methods of calculating excess returns in function \code{\link{Return.excess}}.

See \code{\link[fPortfolio]{pfolioReturn}} in package \code{fPortfolio} for calculating returns from a portfolio of instruments with a combination of historical returns and a weighting vector.  See package \code{\link[portfolio]{portfolio}} for a more sophisticated class-based collection of positions into a historical portfolio, allowing you to track changes in the composition of a portfolio over time.

Returns and risk may be annualized as a way to simplify comparison over longer time periods.  Although it requires a bit of estimating, such aggregation is popular because it offers a reference point for easy comparison.  Examples are in \code{\link{Return.annualized}}, \code{\link{sd.annualized}}, and \code{\link{SharpeRatio.annualized}}.

Basic measures of performance tend to treat returns as independent observations.  In this case, the entirety of R's base is applicable to such analysis.

Some basic statistics we have collected in \code{\link{table.MonthlyReturns}} include:
\cr
\tabular{ll}{
\code{\link{mean}}  \tab  arithmetic mean \cr
\code{\link{mean.geometric}} \tab geometric mean \cr
\code{\link{mean.stderr}} \tab standard error of the mean (S.E. mean) \cr
\code{\link{mean.LCL}} \tab lower confidence level (LCL) of the mean \cr
\code{\link{mean.UCL}} \tab upper confidence level (UCL) of the mean \cr
\code{\link{quantile}}  \tab for calculating various quantiles of the distribution \cr
\code{\link{min}}  \tab minimum return \cr
\code{\link{max}}  \tab maximum return \cr
\code{\link{range}} \tab range of returns \cr
\code{length(R)}  \tab number of observations \cr
\code{sum(is.na(R))} \tab number of NA's \cr
}

\emph{Modern Portfolio Theory (MPT)} \cr
Modern Portfolio Theory is the collection of tools and techniques by which a risk-averse investor may construct an optimal portfolio.  It was pioneered by Markowitz's ground breaking 1952 paper \cite{Portfolio Selection}.  It also encompasses CAPM, below, the efficient market hypothesis, and all forms of quantitative portfolio construction and optimization.

\emph{The Capital Asset Pricing Model (CAPM)}, initially developed by \cite{William Sharpe in 1964}, provides a justification for passive or index investing by positing that assets that are not on the efficient frontier will either rise or lower in price until they are on the efficient frontier of the market portfolio. The \code{\link{CAPM.RiskPremium}} on an investment is the measure of how much the asset's performance differs from the risk free rate.  Negative Risk Premium generally indicates that the investment is a bad investment, and the money should be allocated to the risk free asset or to a different asset with a higher risk premium.  \code{\link{CAPM.alpha}} is the degree to which the assets returns are not due to the return that could be captured from the market. Conversely, \code{\link{CAPM.beta}} describes the portions of the returns of the asset that could be directly attributed to the returns of a passive investment in the benchmark asset. The Capital Market Line \code{\link{CAPM.CML}} relates the excess expected return on an efficient market portfolio to its risk (represented in CAPM by \code{\link[stats]{sd}} ).  The slope of the CML, \code{\link{CAPM.CML.slope}}, is the Sharpe Ratio for the market portfolio. The Security Market Line is constructed by calculating the line of \code{\link{CAPM.RiskPremium}} over \code{\link{CAPM.beta}}.  For the benchmark asset this will be 1 over the risk premium of the benchmark asset. The slope of the SML, primarily for plotting purposes, is given by \code{\link{CAPM.SML.slope}}. CAPM is a market equilibrium model or a general equilibrium theory of the relation of prices to risk, but it is usually applied to partial equilibrium portfolios, which can create (sometimes serious) problems in valuation.

The performance premium provided by an investment over a passive strategy (the benchmark) is provided by \code{\link{ActivePremium}}, which is the investment's annualized return minus the benchmark's annualized return. A closely related measure is the \code{\link{TrackingError}}, which  measures the unexplained portion of the investment's performance relative to a benchmark. The \code{\link{InformationRatio}} of an Investment in a MPT or CAPM framework is the Active Premium divided by the Tracking Error.  Information Ratio may be used to rank investments in a relative fashion.

We have also included a function to compute the \code{\link{KellyRatio}}.  The Kelly criterion, properly applied, will maximize log-utility of returns and avoid risk of ruin.  Utilizing the Kelly Criterion to determine leverage or investment size on a strategy is guaranteed to avoid risk of ruin and eventually (over a long enough time horizon) create wealth, but it may be incredibly volatile, with a N\% chance of being down N\% at some point in time. Even when an investor or analyst does not intend to utilize the Kelly criterion as part of the investment sizing strategy, it can be used as a stack-ranking method like \code{\link{InformationRatio}} to describe the \dQuote{edge} an investment would have over a random strategy or distribution.

 \code{\link{InformationRatio}}, \code{\link{KellyRatio}}, \code{\link{SharpeRatio}}, \code{\link{SortinoRatio}}, \code{\link{UpsidePotentialRatio}}, Spearman Rank Correlation provided by \code{\link[Hmisc]{rcorr}} and other methods discussed here are all methods of rank-ordering relative performance. \cite{Alexander and Dimitriu(2004) in \dQuote{The Art of Investing in Hedge Funds}} show that relative rankings across multiple pricing methodologies may be positively correlated with each other and with expected returns.  This is quite an important finding because it shows that multiple methods of predicting returns and risk which have underlying measures and factors that are not directly correlated to another measure or factor will still produce widely similar quantile rankings, so that the \dQuote{buckets} of target instruments will have significant overlap.  This observation specifically supports the point made early in this document regarding \dQuote{accretion of the evidence} for a positive or negative investment decision.

It is often valuable when evaluating an investment to know whether the instrument that you are examining follows a normal distribution.  One of the first methods to determine how close the asset is to a normal or log-normal distribution is to visually look at your data.  Both \code{\link{chart.QQPlot}} and \code{\link{chart.Histogram}} will quickly give you a feel for whether or not you are looking at a normally distributed return history.  Differences between \code{\link{var}} and \code{\link{SemiVariance}} will help you identify \code{\link{skewness}} in the returns.  Skewness measures the degree of asymmetry in the return distribution.  Positive Skewness indicates that more of the returns are positive, negative skewness indicates that more of the returns are negative.  An investor should in most cases prefer a positively skewed asset to a similar (style, industry, region) asset that has a negative skewness. Kurtosis measures the concentration of the returns in any given part of the distribution (as you should see visually in a histogram).  The \code{\link{kurtosis}} function will by default return what is referred to as \dQuote{excess kurtosis}, where 0 is a normal distribution, other methods of calculating kurtosis than \code{method="excess"} will set the normal distribution at a value of 3.  In general a rational investor should prefer an asset with a low to negative excess kurtosis, as this will indicate more predictable returns (the major exception is generally a combination of high positive skewness and high excess kurtosis).  If you find yourself needing to analyze the distribution of complex or non-smooth asset distributions, the \code{nortest} package has several advanced statistical tests for analyzing the normality of a distribution.

One question that an investor or researcher is often required to answer revolves around how to diversify a portfolio.  In a mean-variance world, an asset is a diversifier if it lowers the variance (and hopefully also raises the mean return) of the portfolio.  In a world with more nuanced risk metrics, you may also test the target portfolio to see if it has lower measures of risk than the current portfolio. If you want to do more analysis, we have provided functions to calculate the higher moments and co-moments of the distribution.  We have already discussed \code{\link{skewness}} and \code{\link{kurtosis}} provided by package \code{fBasics}.  Increasing skewness and decreasing kurtosis of the target portfolio probably marks a good diversifier.  You can also work on minimizing the co-moments and systematic co-moments of the distribution to improve the resilience of the portfolio to multiple different market conditions in much the way that minimizing covariance and systematic beta will mark a well diversified portfolio in a mean-variance framework.  See \code{\link{CoMoments}} for the co-moments and \code{\link{BetaCoMoments}} for the systematic standardized co-moments. Other diversification potential in a portfolio is not strictly econometric, and follow the classic asset delineations of asset type, size, industry, growth/value, currency, or geographic region, all of which are well-covered elsewhere.

We have covered multiple methods of performance \emph{analysis} in this summary section, but it is also important to note that we have \emph{not} covered performance \emph{attribution}.  We have not examined tools for determining the sources and causes for the returns, or the underlying causes for the risks of an instrument (more on Risk Measurement and prediction below).  There is a significant amount of academic literature on identifying these sources of risk or returns.  Much of it falls into the field of \dQuote{factor analysis} where \dQuote{risk factors} are used to retrospectively explain sources of risk, and through regression and other analytical methods \emph{predict} future period returns and risk based on factor drivers.  These are well covered in chapters on factor analysis in \cite{Zivot and Wang(2006)} and also in the \R functions \code{\link{factanal}} for basic factor analysis and \code{\link{princomp}} for Principal Component Analysis.  The authors feel that financial engineers and analysts would benefit from some wrapping of this functionality focused on finance, but the capabilities already available from the base functions are quite powerful.
}

\section{Style Analysis}{
Style analysis of fund managers is another aspect of performance attribution.  We have added a four functions to \code{PerformanceAnalytics} which begin to address the need for doing style analysis of fund managers or other fund-like products or indexes:

  \tabular{rl}{
    \code{\link{chart.RollingStyle}} \tab display style weightings over rolling periods calculated by \code{\link{style.fit}} \cr
    \code{\link{chart.Style}} \tab display style weightings calculated by \code{\link{style.fit}} \cr
    \code{\link{style.fit}} \tab calculate and display effective style weights  \cr
    \code{\link{style.QPfit}} \tab calculate and display effective style weights using quadratic programming \cr
  }

These functions calculate style weights using an asset class style model as described in detail in Sharpe (1992).  The use of quadratic programming to determine a fund's exposures to the changes in returns of major asset classes is usually refered to as "style analysis".

The following functions calculate effective style weights and display the results in a bar chart.  'chart.Style' calculates and displays style weights calculated over a single period. 'chart.RollingStyle' calculates and displays those weights in rolling windows through time.  'style.fit' manages the calculation of the weights by method.  'style.QPfit' calculates the specific constraint case that requires quadratic programming.
}

\section{Robust Data Cleaning}{
The functions \code{\link{Return.clean}} and \code{\link{clean.boudt}} implement statistically robust data cleaning methods tuned to portfolio construction and risk analysis and prediction in financial time series while trying to avoid some of the pitfalls of standard robust statistical methods.

The primary value of data cleaning lies in creating a more
robust and stable estimation of the distribution generating the
large majority of the return data. The increased robustness and
stability of the estimated moments using cleaned data should be
used for portfolio construction. If an investor wishes to
have a more conservative risk estimate, cleaning may not be
indicated for risk monitoring. 

In actual practice, it is probably best to back-test the out-of-sample results of
both cleaned and uncleaned series to see what works best when forecasting risk with the
particular combination of assets under consideration.
}

\section{Summary Tabular Data}{
Summary statistics are then the necessary aggregation and reduction of (potentially thousands) of periodic return numbers. Usually these statistics are most palatable when organized into a table of related statistics, assembled for a particular purpose.  A common offering of past returns organized by month and cumulated by calendar year is usually presented as a table, such as in \code{\link{table.CalendarReturns}}.  Adding benchmarks or peers alongside the annualized data is helpful for comparing returns in calendar years.

When we started this project, we debated whether such tables would be broadly useful or not.  No reader is likely to think that we captured the precise statistics to help their decision. We merely offer these as a starting point for creating your own.  Add, subtract, do whatever seems useful to you.  If you think that your work may be useful to others, please consider sharing it so that we may include it in a future version of this package.

Other tables for comparison of related groupings of statistics discussed elsewhere:
  \tabular{rl}{
    \code{\link{table.AnnualizedReturns}} \tab Annualized Returns Summary: Statistics and Stylized Facts \cr
    \code{\link{table.Arbitrary}} \tab wrapper function for combining arbitrary function list into a table \cr
    \code{\link{table.Autocorrelation}} \tab table for calculating the first six autocorrelation coefficients and significance \cr
    \code{\link{table.CalendarReturns}} \tab Monthly and Calendar year Return table \cr
    \code{\link{table.CAPM}} \tab Asset-Pricing Model Summary: Statistics and Stylized Facts \cr
    \code{\link{table.Correlation}} \tab calculate correlalations of multicolumn data \cr
    \code{\link{table.DownsideRisk}} \tab Downside Risk Summary: Statistics and Stylized Facts \cr
    \code{\link{table.Drawdowns}} \tab Worst Drawdowns Summary: Statistics and Stylized Facts \cr
    \code{\link{table.HigherMoments}} \tab Higher Moments Summary: Statistics and Stylized Facts \cr
    \code{\link{table.MonthlyReturns}} \tab Monthly Returns Summary: Statistics and Stylized Facts \cr
    \code{\link{table.Returns}} \tab Monthly and Calendar year Return table \cr
    \code{\link{table.RollingPeriods}} \tab Rolling Periods Summary: Statistics and Stylized Facts \cr
  }
}

\section{Charts and Graphs}{
Graphs and charts can also help to organize the information visually. Our goal in creating these charts was to simplify the process of creating well-formatted charts that are used often in portfolio analysis, and to create print-quality graphics that may be used in documents for consumption by non-analysts or researchers. \R's graphics capabilities are substantial, but the simplicity of the output of \R default graphics functions such as \code{\link[graphics]{plot}} does not always compare well against graphics delivered with commercial asset or portfolio analysis from places such as MorningStar or PerTrac.

The cumulative returns or wealth index is usually the first thing displayed, even though neither conveys much information.  See \code{\link{chart.CumReturns}}.  Individual period returns may be helpful for identifying problematic periods, such as in \code{\link{chart.Bar}}.  Risk measures can be helpful when overlaid on the period returns, to display the bounds at which losses may be expected.  See \code{\link{chart.BarVaR}} and the following section on Risk Analysis.  More information can be conveyed when such charts are displayed together, as in \code{\link{charts.PerformanceSummary}}, which combines the performance data with detail on downside risk (see \code{\link{chart.Drawdown}}).

Two-dimensional charts can also be useful while remaining easy to understand.  \code{\link{chart.Scatter}} is a utility scatter chart with some additional attributes that are used in \code{\link{chart.RiskReturnScatter}}.  Overlaying Sharpe ratio lines or boxplots helps to add information about relative performance along those dimensions. The relative performance through time of two assets can be plotted with \code{\link{chart.RelativePerformance}}.  This plot displays the ratio of the cumulative performance at each point in time and makes periods of under- or out-performance easy to see.  The value of the chart is less important than the slope of the line.  If the slope is positive, the first asset is outperforming the second, and vice verse.  Affectionately known as the Canto chart, it was used effectively in Canto (2006).

For distributional analysis, a few graphics may be useful.  \code{\link{chart.Boxplot}} is an example of a graphic that is difficult to create in Excel and is under-utilized as a result.  A boxplot of returns is, however, a very useful way to instantly observe the shape of large collections of asset returns in a manner that makes them easy to compare to one another.  \code{\link{chart.Histogram}} and \code{\link{chart.QQPlot}} are two charts originally ported from Rmetrics and now substantially expanded in PerformanceAnalytics.

Rolling performance is typically used as a way to assess stability of a return stream.  Although perhaps it doesn't get much credence in the financial literature as it derives from work in digital signal processing, many practitioners find it a useful way to examine and segment performance and risk periods.  See \code{\link{chart.RollingPerformance}}, which is a way to display different metrics over rolling time periods.  \code{\link{chart.RollingMean}} is a specific example of a rolling mean and one standard deviation bands.  A group of related metrics is offered in \code{\link{charts.RollingPerformance}}.  These charts use utility functions such as \code{\link[zoo]{rollapply}}, \code{\link[zoo]{window}}, and \code{\link{apply.rolling}}.  \code{\link{chart.SnailTrail}} is a scatter chart that shows how rolling calculations of annualized return and annualized standard deviation have proceeded through time where the color of lines and dots on the chart diminishes with respect to time. \code{\link{chart.RollingCorrelation}} shows how correlations change over rolling periods.  \code{\link{chart.RollingRegression}} displays the coefficients of a linear model fitted over rolling periods.  A group of charts in \code{\link{charts.RollingRegression}} displays alpha, beta, and R-squared estimates in three aligned charts in a single device. 

\code{\link{chart.StackedBar}} creates a stacked column chart with time on the horizontal axis and values in categories.  This kind of chart is commonly used for showing portfolio 'weights' through time, although the function will plot any values by category.  This is a primitive function and is expected to improve.

A number of charts or groups of charts that remain unfinished or were experimental and may be modified or removed in the future.  \code{\link{chart.Correlation}}, and \code{\link{chart.Correlation.color}} are examples.  The latter two are slight modifications of code from the package \code{MASS} and the \R \code{base}.  See their documentation for more information.

We have been greatly inspired by other peoples' work, some of which is on display at \url{http://addictedtor.free.fr/}.  Particular inspiration came from Dirk Eddelbuettel and John Bollinger for their work at \url{http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=65}. Those interested in price charting in \R should also look at the \code{quantmod} package.
}

\section{Risk Analysis}{
Many methods have been proposed to measure, monitor, and control the risks of a diversified portfolio. Perhaps a few definitions are in order on how different risks are generally classified. \emph{Market Risk} is the risk to the portfolio from a decline in the market price of instruments in the portfolio.  \emph{Liquidity Risk} is the risk that the holder of an instrument will find that a position is illiquid, and will incur extra costs in unwinding the position resulting in a less favorable price for the instrument. In  extreme cases of liquidity risk, the seller may be unable to find a buyer for the instrument at all, making the value unknowable or zero.  \emph{Credit Risk} is also sometimes called \emph{Default Risk}, or the risk that promised payments on a loan or bond will not be made, or that a convertible instrument will not be converted in a timely manner or at all.  There are also additional \emph{Counterparty Risks} in manual or over the counter markets, and in many complex derivatives.  Tools have evolved to measure all these different components of risk.  Processes must be put into place inside a firm to monitor the changing risks in a portfolio, and to control the magnitude of risks.  For an extensive treatment of these topics, see Litterman, Gumerlock, et. al.(1998).

The simplest risk measure in common use is volatility, usually modeled quantitatively with a univariate standard deviation on a portfolio.  See \code{\link[stats]{sd}} .  Volatility or Standard Deviation is an appropriate risk measure when the distribution of returns is normal or resembles a random walk, and may be annualized using \code{\link{sd.annualized}}, or the equivalent function \code{\link{sd.multiperiod}} for scaling to an arbitrary number of periods.  Many assets, including hedge funds, commodities, options, and even most common stocks over a sufficiently long period, do not follow a normal distribution.  For such common but non-normally distributed assets, a more sophisticated approach than standard deviation/volatility is required to adequately model the risk.

Markowitz, in his Nobel acceptance speech and in several papers, proposed that \code{\link{SemiVariance}} would be a better measure of risk than variance.  See \cite{Zin, Markowitz, Zhao(2006)} \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=910640}.  This measure is also called \code{\link{SemiDeviation}}.  The more general case of downside deviation is implemented in the function \code{\link{DownsideDeviation}}, as proposed by \cite{Sortino and Price(1994)}, where the minimum acceptable return (MAR) is a parameter to the function.  It is interesting to note that variance and mean return can produce a smoothly elliptical efficient frontier for portfolio optimization utilizing \code{\link[quadprog]{solve.QP}} or \code{\link[tseries]{portfolio.optim}} or \code{\link[fPortfolio]{MarkowitzPortfolio}}. Use of semivariance or many other risk measures will not necessarily create a smooth ellipse, causing significant additional difficulties for the portfolio manager trying to build an optimal portfolio.  We'll leave a more complete treatment and implementation of portfolio optimization techniques for another time.

Another very widely used downside risk measures is analysis of drawdowns, or loss from peak value achieved. The simplest method is to check the \code{\link{maxDrawdown}}, as this will tell you the worst cumulative loss ever sustained by the asset.  If you want to look at all the drawdowns, you can \code{\link{findDrawdowns}} and \code{\link{sortDrawdowns}} in order from worst/major to smallest/minor. The \code{\link{UpDownRatios}} function will give you some insight into the impacts of the skewness and kurtosis of the returns, and letting you know how length and magnitude of up or down moves compare to each other.  You can also plot drawdowns with \code{\link{chart.Drawdown}}.

One of the most commonly used and cited measures of the risk/reward tradeoff of an investment or portfolio is the \code{\link{SharpeRatio}}, which measures return over standard deviation.  If you are comparing multiple assets using Sharpe, you should use \code{\link{SharpeRatio.annualized}}. It is important to note that William Sharpe now recommends \code{\link{InformationRatio}} preferentially to the original Sharpe Ratio. The \code{\link{SortinoRatio}} utilizes mean return over \code{\link{DownsideDeviation}} below the MAR as the risk measure to produce a similar ratio that is more sensitive to downside risk. Sortino later enhanced his ideas to utilize upside returns for the numerator and \code{\link{DownsideDeviation}} as the denominator in \code{\link{UpsidePotentialRatio}}. \cite{Favre and Galeano(2002)} propose utilizing the ratio of expected excess return over the Cornish-Fisher \code{\link{VaR}} to produce \code{\link{SharpeRatio.modified}}. \code{\link{TreynorRatio}} is also similar to the Sharpe Ratio, except it uses \code{\link{CAPM.beta}} in place of the volatility measure to produce the ratio of the investment's excess return over the beta.

One of the newest statistical methods developed for analyzing the risk of financial instruments is \code{\link{Omega}}.  Omega analytically constructs a cumulative distribution function, in a manner similar to \code{\link{chart.QQPlot}}, but then extracts additional information from the location and slope of the derived function at the point indicated by the risk quantile that the researcher is interested in.  Omega seeks to combine a large amount of data about the shape, magnitude, and slope of the distribution into one method.  Omega is still a very new method, and the academic literature is still exploring the best manner to utilize Omega in a risk measurement and control process, or in portfolio construction.

Any risk measure should be viewed with suspicion if there are not a large number of historical observations of returns for the asset in question available.  Depending on the measure, how many observations are required will vary greatly from a statistical standpoint.  As a heuristic rule, ideally you will have data available on how the instrument performed through several economic cycles and shocks.  When such a long history is not available, the investor or researcher has several options.  A full discussion of the various approaches is beyond the scope of this introduction, so we will merely touch on several areas that an interested party may wish to explore in additional detail. Examining the returns of assets with a similar style, industry, or asset class to which the asset in question is highly correlated and shares other characteristics can be quite informative.  Factor analysis may be utilized to uncover specific risk factors where transparency is not available. Various resampling (see \code{\link[tseries]{tsbootstrap}}) and simulation methods are available in \R to construct an artificially long distribution for testing.  If you use a method such as Monte Carlo simulation or the bootstrap, it is often valuable to use \code{\link{chart.Boxplot}} to visualize the different estimates of the risk measure produced by the simulation, to see how small (or wide) a range the estimates cover, and thus gain a level of confidence with the results.  Proceed with extreme caution when your historical data is lacking.  Problems with lack of historical data are a major reason why many institutional investors will not invest in an alternative asset without several years of historical return data available.
}

\section{Value at Risk - VaR}{
\emph{Traditional mean-VaR}:
In the early 90's, academic literature started referring to \dQuote{value at risk}, typically written as VaR. Take care to capitalize VaR in the commonly accepted manner, to avoid confusion with var (variance) and VAR (vector auto-regression).  With a sufficiently large data set, you may choose to utilize a non-parametric VaR estimation method using the historical distribution and the probability quantile of the distribution calculated using \code{\link{qnorm}}. The negative return at the correct quantile (usually 95\% or 99\%), is the non-parametric VaR estimate.  J.P. Morgan's RiskMetrics parametric mean-VaR was published in 1994 and this methodology for estimating parametric mean-VaR has become what people are generally referring to as \dQuote{VaR} and what we have implemented as \code{\link{VaR}}.  See \cite{Return to RiskMetrics: Evolution of a Standard}\url{http://www.riskmetrics.com/r2rovv.html}, \code{fPortfolio} has also implemented traditional mean-VaR as the \code{\link[fPortfolio]{VaR}} function.  Parametric traditional VaR does a better job of accounting for the tails of the distribution by more precisely estimating the tails below the risk quantile.  It is still insufficient if the assets have a distribution that varies widely from normality.

The \R package \code{VaR} contains methods for simulating and estimating lognormal \code{\link[VaR]{VaR.norm}} and generalized Pareto \code{\link[VaR]{VaR.gpd}} distributions to overcome some of the problems with nonparametric or parametric mean-VaR calculations on a limited sample size.  There is also a \code{\link[VaR]{VaR.backtest}} function to apply simulation methods to create a more robust estimate of the potential distribution of losses.  The VaR package also provides plots for its functions.

\emph{Conditional VaR and Expected Shortfall}:
The \code{fPortfolio} package has implemented Conditional Value at Risk, also called Expected Shortfall (not to be confused with shortfall probability, which is much less useful), in functions \code{\link[fPortfolio]{CVaR}} and \code{\link[fPortfolio]{CVaRplus}}.  Expected Shortfall attempts to measure the magnitude of the average loss exceeding the traditional mean-VaR. Expected Shortfall has proven to be a reasonable risk predictor for many asset classes.  We have provided traditional Gaussian and modified Cornish-Fisher measures of Expected Shortfall in \code{\link{GES.MM}} and \code{\link{mES.MM}}. See \cite{Uryasev(2000)} and \cite{Sherer and Martin(2005)} for more information on Conditional Value at Risk and Expected Shortfall.  Please note that your milage will vary; expect that values obtained from the normal distribution may differ radically from the real situation, depending on the assets under analysis.  There is still work to do in wrapping all VaR and ES functiuons into a coherent set that are easily called with a "method" argument, as some of the more mature functions in PerformanceAnalytics utilize.

\emph{Modified Cornish-Fisher VaR}:
The limitations of traditional mean-VaR are all related to the use of a symmetrical distribution function.  Use of simulations, resampling, or Pareto distributions all help in making a more accurate prediction, but they are still flawed for assets with significantly non-normal (skewed and/or kurtotic) distributions. \cite{Huisman(1999)} and \cite{Favre and Galleano(2002)} propose to overcome this extensively documented failing of traditional VaR by directly incorporating the higher moments of the return distribution into the VaR calculation.  This new VaR measure incorporates skewness and kurtosis via an analytical estimation using a Cornish-Fisher (special case of a Taylor) expansion. The resulting measure is referred to variously as \dQuote{Cornish-Fisher VaR} or \dQuote{Modified VaR}.  We provide this measure as function \code{\link{VaR}} with equivalent alias \code{\link{VaR}}, Modified VaR produces the same results as traditional mean-VaR when the return distribution is normal, so it may be used as a direct replacement.  Many papers in the finance literature have reached the conclusion that Modified VaR is a superior measure, and may be substituted in any case where mean-VaR would previously have been utilized.

\emph{Multivariate extensions to risk measures}:
We have extened all moments calculations to woork in a multivariate portfolio context.  In a portfolio context the multivariate moments are generally to be preferred to their univariate counterparts, so that all information is available to subsequent calculations.  See \code{\link{MultivariateMoments}} and \code{\link{MultivariateRisk}}

\emph{Marginal, Incremental, and Component VaR}:
Marginal VaR is the difference between the VaR of the portfolio without the asset in question and the entire portfolio.  The \code{\link{VaR}} function calculates Marginal VaR for all instruments in the portfolio if you set \code{method="marginal"}. Marginal VaR as provided here may use traditional mean-VaR or Modified VaR for the calculation. Per \cite{Artzner,et.al.(1997)} properties of a coherent risk measure include subadditivity (risks of the portfolio should not exceed the sum of the risks of individual components) as a significantly desirable trait.  VaR measures, including Marginal VaR, on individual components of a portfolio are \emph{not} subadditive. Clearly, a general subbadditive risk measure for downside risk is required.  Incremental or Component VaR attempt to provide just such a measure.  In Incremental or Component VaR, the Component VaR value for each element of the portfolio will sum to the total VaR of the portfolio.  Several EDHEC papers suggest utilizing Modified VaR instead of mean-VaR in the Incremental and Component VaR calculation.  We have succeeded in implementing Component VaR and ES calculations that utilize Modified Cornish-Fisher VaR, historical decomposition, and kernel estimators.  You may access these with \code{\link{VaR}} or \code{\link{ES}} by setting the appropriate \code{portfolio_method} and \code{method} arguments.


\code{\link{chart.VaRSensitivity}} Creates a chart of Value-at-Risk estimates by confidence interval for multiple methods. Useful for comparing a calculated VaR method to the historical VaR, it may also be used to visually examine whether the VaR method \dQuote{breaks down} or gives nonsense results at a certain threshold.

Which VaR measure to use will depend greatly on the portfolio and instruments being analyzed.  If there is any generalization to be made on VaR measures, this author will agree with \cite{Bali and Gokcan(2004)} who conclude that \dQuote{the VaR estimations based on the generalized Pareto distribution and the Cornish-Fisher approximation perform best}.
}

\section{Moments and Co-moments}{
Analysis of financial time series often involves evaluating their mathematical moments.  While \code{\link{var}} and \code{\link{cov}} for variance has always been available, and \code{\link[fBasics]{fBasics}} in Rmetrics provided functions for skewness and kurtosis, a larger suite of multivariate moments calculations was not available in \R .  We have now implemented multivariiate moments and co-moments and their beta or systematic comoments in \code{PerformanceAnalytics}.
}

\section{Wrapper and Utility Functions}{
\R is a very powerful environment for manipulating data.  It can also be quite confusing to a user more accustomed to Excel or even MatLAB.  As such, we have written some wrapper functions that may aid you in coercing data into the correct forms or finding data that you need to use regularly.  To simplify the management of multiple-source data stored in \R in multiple data formats, we have provided \code{\link{checkData}}.  This function will attempt to coerce data in and out of \R's multitude of mostly fungible data classes into the class required for a particular analysis.  In general, the use of the data-coercion function has been hidden inside the business functions provided.  \code{\link{checkData}} may also save you time and trouble in your own code and functions outside of the functionality provided by the \code{PerformanceAnalytics} library.

When you are analyzing relative or absolute performance of investments, you need to analyze returns, but much data is available only as prices.  We have provided the simple wrapper function \code{\link{CalculateReturns}} to address this by taking a stream of prices and calculating simple or compounded returns from the price vector. The excellent \code{tseries} library includes the function \code{\link[tseries]{get.hist.quote}} for retrieving market data from online sources.  Many of the functions in PerformanceAnalytics require either a benchmark or a risk free rate, and many practitioners will not have access to a formal database of historical returns.  To facilitate the examples and provide an example of how to retrieve and coerce the data, we have provided functions for S&P 500 returns in the \code{\link{download.SP500PriceReturns}} and the 13-day US Treasury Bill in \code{\link{download.RiskFree}}.

\R's built-in \code{\link{apply}} function in enormously powerful, but is can be tricky to use with timeseries data, so we have provided wrapper functions to \code{\link{apply.fromstart}} and \code{\link{apply.rolling}} to make handling of \dQuote{from inception} and \dQuote{rolling window} calculations easier.

}

\section{Further Work}{

Optimization

Performance Attribution

Factor Analysis

Hedge Selection and Analysis

Shock/Scenario Analysis

We have attempted to standardize function parameters and variable names, but more work exists to be done here.

There are functions included in this package that would benefit from more complex examples.

Any comments, suggestions, or code patches are invited.

If you've implemented anything in the list above, please consider donating it for inclusion in a later version of this package.
}

\author{
Brian G. Peterson \cr
Peter Carl \cr

Maintainer: Brian G. Peterson \email{brian@braverock.com}
}

\references{
Bacon, Carl. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. \cr
Canto, Victor. \emph{Understanding Asset Allocation}. FT Prentice Hall. 2006. \cr
Lhabitant, F. \emph{Hedge Funds: Quantitative Insights}. Wiley. 2004. \cr
Litterman, R., Gumerlock R., et. al. \emph{The Practice of Risk Management: Implementing Processes for Managing Firm-Wide Market Risk}. Euromoney. 1998. \cr
Murrel, Paul. \emph{R Graphics}. Chapman and Hall. 2006.  \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
Scherer, B. and Martin, D. \emph{Modern Portfolio Optimization}. Springer. 2005. \cr
Shumway, R. and Stoffer, D. emph{Time Series Analysis and it's Applications, with R examples}, Springer, 2006.
Tsay, R. \emph{Analysis of Financial Time Series}. Wiley. 2001. \cr
Zivot, E. and Wang, Z. \emph{Modeling Financial Time Series with S-Plus: second edition}. Springer. 2006. \cr
}

\seealso{
    CRAN task view on Empirical Finance \cr \url{http://cran.r-project.org/src/contrib/Views/Econometrics.html}

    Grant Farnsworth's Econometrics in R \cr \url{http://cran.r-project.org/doc/contrib/Farnsworth-EconometricsInR.pdf}

    Collection of R charts and graphs \cr \url{http://addictedtor.free.fr/graphiques/}

}

\section{Acknowledgments}{
Data series \code{\link{edhec}} used in \code{PerformanceAnalytics} and related publications with the kind permission of the EDHEC Risk and Asset Management Research Center. \cr
\url{http://www.edhec-risk.com/indexes/pure_style}

Kris Boudt was instrumental in our research on component risk for portfolios with non-normal distributions, and is responsible for much of the code for multivariate moments and comoments.

Jeff Ryan, as the primary author of xts, is owed a huge debt (which he would be happy to collect over a pint) by the entire R finance community for creating xts, upon which much of PerformanceAnalytics depends.

Prototypes of the drawdowns functionality were provided by Sankalp Upadhyay, and modified with permission. Stephan Albrecht provided detailed feedback on the Getmansky/Lo Smoothing Index.  Diethelm Wuertz provided prototypes of modified VaR and skewness and kurtosis functions (and is of course the maintainer of the RMetrics suite of pricing and optimization functions)

Thanks to Joe Wayne Byers and Dirk Eddelbuettel for comments on early versions of these functions, and to Khanh Nguyen and Ryan Sheftel for careful testing and detailed problem reports.

Thanks to the R-SIG-Finance community without whom this package would not be possible.  We are indebted to the R-SIG-Finance community for many helpful suggestions, bugfixes, and requests.
}


\keyword{ package }
