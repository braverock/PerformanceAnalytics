\name{PerformanceAnalytics-package}
\alias{PerformanceAnalytics-package}
\alias{PerformanceAnalytics}
\docType{package}
\title{
Econometric tools for performance and risk analysis.
}

\description{
\kbd{PerformanceAnalytics} provides an \R package of econometric functions for performance and risk analysis of financial instruments or portfolios. This package aims to aid practitioners and researchers in using the latest research for analysis of both normally and non-normally distributed return streams.

We created this package to include functionality that has been appearing in the academic literature on performance analysis and risk over the past several years, but had no functional equivalent in \R.  In doing so, we also found it valuable to have wrappers for some functionality with good defaults and naming consistent with common usage in the finance literature.  

In general, this package requires return (rather than price) data. Almost all of the functions will work with any periodicity, from annual, monthly, daily, to even minutes and seconds, either regular or irregular.

The following sections cover Time Series Data, Performance Analysis, Risk Analysis (with a separate treatment of VaR), Summary Tables of related statistics, Charts and Graphs, a variety of Wrappers and Utility functions, and some thoughts on work yet to be done.

In this summary, we attempt to provide an overview of the capabilities provided by \kbd{PerformanceAnalytics} and pointers to other literature and resources in \R useful for performance and risk analysis.  We hope that this summary and the accompanying package and documentation partially fill a hole in the tools available to a financial engineer or analyst.
}

\section{Time Series Data}{
Not all, but many of the measures in this package require time series data.  \kbd{PerformanceAnalytics} uses the \code{\link[xts]{xts}} package for managing time series data for several reasons.  Besides being fast and efficient, \code{\link[xts]{xts}} includes functions that test the data for periodicity and draw attractive and readable time-based axes on charts.  Another benefit is that \code{\link[xts]{xts}} provides compatability with Rmetrics' \code{\link[timeSeries]{timeSeries}}, \code{\link[zoo]{zoo}} and other time series classes, such that \kbd{PerformanceAnalytics} functions that return a time series will return the results in the same format as the object that was passed in.  Jeff Ryan and Josh Ulrich, the authors of \code{\link[xts]{xts}}, have been extraordinarily helpful to the development of \kbd{PerformanceAnalytics} and we are very greatful for their contributions to the community.  The \code{\link[xts]{xts}} package extends the excellent \code{\link[zoo]{zoo}} package written by Achim Zeileis and Gabor Grothendieck. \code{\link[zoo]{zoo}} provides more general time series support, whereas \code{\link[xts]{xts}} provides functionality that is specifically aimed at users in finance.

Users can easily load returns data as time series for analysis with \kbd{PerformanceAnalytics} by using the \code{\link{Return.read}} function.  The \code{\link{Return.read}} function loads csv files of returns data where the data is organized as dates in the first column and the returns for the period in subsequent columns.  See \code{\link[zoo]{read.zoo}} and \code{\link[xts]{as.xts}} if more flexibility is needed.  

The functions described below assume that input data is organized with asset returns in columns and dates represented in rows.  All of the metrics in \kbd{PerformanceAnalytics} are calculated by column and return values for each column in the results.  This is the default arrangement of time series data in \code{xts}.

Some sample data is available in the \code{\link{managers}} dataset. It is an xts object that contains columns of monthly returns for six hypothetical asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge fund index, the S&P 500 total returns, and total return series for the US Treasury 10-year bond and 3-month bill. Monthly returns for all series end in December 2006 and begin at different periods starting from January 1996. That data set is used extensively in our examples and should serve as a model for formatting your data.

For retrieving market data from online sources, see \code{quantmod}'s \code{\link[quantmod]{getSymbols}} function for downloading prices and \code{\link[quantmod]{chartSeries}} for graphing price data.  Also see the \code{tseries} package for the function \code{\link[tseries]{get.hist.quote}}. Look at \code{xts}'s \code{\link[xts]{to.period}} function to rationally coerce irregular price data into regular data of a specified periodicity.  The \code{\link{aggregate}} function has methods for \code{tseries} and \code{zoo} timeseries data classes to rationally coerce irregular data into regular data of the correct periodicity.  

Finally, see the function \code{\link{Return.calculate}} for calculating returns from prices.
}

\section{Performance Analysis}{
The literature around the subject of performance analysis seems to have exploded with the popularity of alternative assets such as hedge funds, managed futures, commodities, and structured products. Simpler tools that may have seemed appropriate in a relative investment world seem inappropriate for an absolute return world.  Risk measurement, which is nearly inseparable from performance assessment, has become multi-dimensional and multi-moment while trying to answer a simple question: \dQuote{How much could I lose?}  Portfolio construction and risk budgeting are two sides of the same coin: \dQuote{How do I maximize my expected gain and avoid going broke?}  But before we can approach those questions we first have to ask: \dQuote{Is this something I might want in my portfolio?}

With the the increasing availability of complicated alternative investment strategies to both retail and institutional investors, and the broad availability of financial data, an engaging debate about performance analysis and evaluation is as important as ever.  There won't be one \emph{right} answer delivered in these metrics and charts.  What there will be is an accretion of evidence, organized to \emph{assist} a decision maker in answering a specific question that is pertinent to the decision at hand.  Using such tools to uncover information and ask better questions will, in turn, create a more informed investor.

Performance measurement starts with returns.  Traders may object, complaining that \dQuote{You can't eat returns,} and will prefer to look for numbers with currency signs.  To some extent, they have a point - the normalization inherent in calculating returns can be deceiving.  Most of the recent work in performance analysis, however, is focused on returns rather than prices and sometimes called "returns-based analysis" or RBA.  This \dQuote{price per unit of investment} standardization is important for two reasons - first, it helps the decision maker to compare opportunities, and second, it has some useful statistical qualities.  As a result, the \kbd{PerformanceAnalytics} package focuses on returns.  See \code{\link{Return.calculate}} for converting net asset values or prices into returns, either discrete or continuous.  Many papers and theories refer to \dQuote{excess returns}: we implement a simple function for aligning time series and calculating excess returns in \code{\link{Return.excess}}.

\code{\link{Return.portfolio}} can be used to calculate weighted returns for a portfolio of assets.  The function was recently changed to support several use-cases: a single weighting vector, an equal weighted portfolio, periodic rebalancing, or irregular rebalancing.  That replaces functionality that had been split between that function and \code{\link{Return.rebalancing}}.  The function will subset the return series to only include returns for assets for which \code{\link{weights}} are provided.

Returns and risk may be annualized as a way to simplify comparison over longer time periods.  Although it requires a bit of estimating, such aggregation is popular because it offers a reference point for easy comparison.  Examples are in \code{\link{Return.annualized}}, \code{\link{sd.annualized}}, and \code{\link{SharpeRatio.annualized}}.

Basic measures of performance tend to treat returns as independent observations.  In this case, the entirety of R's base is applicable to such analysis.  Some basic statistics we have collected in \code{\link{table.Stats}} include:
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

It is often valuable when evaluating an investment to know whether the instrument that you are examining follows a normal distribution.  One of the first methods to determine how close the asset is to a normal or log-normal distribution is to visually look at your data.  Both \code{\link{chart.QQPlot}} and \code{\link{chart.Histogram}} will quickly give you a feel for whether or not you are looking at a normally distributed return history.  Differences between \code{\link{var}} and \code{\link{SemiVariance}} will help you identify \code{\link{skewness}} in the returns.  Skewness measures the degree of asymmetry in the return distribution.  Positive skewness indicates that more of the returns are positive, negative skewness indicates that more of the returns are negative.  An investor should in most cases prefer a positively skewed asset to a similar (style, industry, region) asset that has a negative skewness. 

Kurtosis measures the concentration of the returns in any given part of the distribution (as you should see visually in a histogram).  The \code{\link{kurtosis}} function will by default return what is referred to as \dQuote{excess kurtosis}, where 0 is a normal distribution, other methods of calculating kurtosis than \code{method="excess"} will set the normal distribution at a value of 3.  In general a rational investor should prefer an asset with a low to negative excess kurtosis, as this will indicate more predictable returns (the major exception is generally a combination of high positive skewness and high excess kurtosis).  If you find yourself needing to analyze the distribution of complex or non-smooth asset distributions, the \code{nortest} package has several advanced statistical tests for analyzing the normality of a distribution.

\emph{Modern Portfolio Theory (MPT)} is the collection of tools and techniques by which a risk-averse investor may construct an \dQuote{optimal} portfolio.  It was pioneered by Markowitz's ground-breaking 1952 paper \cite{Portfolio Selection}.  It also encompasses CAPM, below, the efficient market hypothesis, and all forms of quantitative portfolio construction and optimization.

\emph{The Capital Asset Pricing Model (CAPM)}, initially developed by William Sharpe in 1964, provides a justification for passive or index investing by positing that assets that are not on the efficient frontier will either rise or fall in price until they are. The \code{\link{CAPM.RiskPremium}} is the measure of how much the asset's performance differs from the risk free rate.  Negative Risk Premium generally indicates that the investment is a bad investment, and the money should be allocated to the risk free asset or to a different asset with a higher risk premium.  \code{\link{CAPM.alpha}} is the degree to which the assets returns are not due to the return that could be captured from the market. Conversely, \code{\link{CAPM.beta}} describes the portions of the returns of the asset that could be directly attributed to the returns of a passive investment in the benchmark asset. 

The Capital Market Line \code{\link{CAPM.CML}} relates the excess expected return on an efficient market portfolio to its risk (represented in CAPM by \code{\link[stats]{sd}}).  The slope of the CML, \code{\link{CAPM.CML.slope}}, is the Sharpe Ratio for the market portfolio. The Security Market Line is constructed by calculating the line of \code{\link{CAPM.RiskPremium}} over \code{\link{CAPM.beta}}.  For the benchmark asset this will be 1 over the risk premium of the benchmark asset. The slope of the SML, primarily for plotting purposes, is given by \code{\link{CAPM.SML.slope}}. CAPM is a market equilibrium model or a general equilibrium theory of the relation of prices to risk, but it is usually applied to partial equilibrium portfolios, which can create (sometimes serious) problems in valuation.

One extension to the CAPM contemplates evaluating an active manager's ability to time the market.  Two other functions apply the same notion of best fit to positive and negative market returns, separately.  The \code{\link{CAPM.beta.bull}} is a regression for only positive market returns, which can be used to understand the behavior of the asset or portfolio in positive or 'bull' markets.  Alternatively, \code{\link{CAPM.beta.bear}} provides the calculation on negative market returns. The \code{\link{TimingRatio}} uses the ratio of those to help assess whether the manager has shown evidence that of timing skill.

The performance premium provided by an investment over a passive strategy (the benchmark) is provided by \code{\link{ActivePremium}}, which is the investment's annualized return minus the benchmark's annualized return. A closely related measure is the \code{\link{TrackingError}}, which  measures the unexplained portion of the investment's performance relative to a benchmark. The \code{\link{InformationRatio}} of an investment in a MPT or CAPM framework is the Active Premium divided by the Tracking Error.  Information Ratio may be used to rank investments in a relative fashion.

We have also included a function to compute the \code{\link{KellyRatio}}.  The Kelly criterion applied to position sizing will maximize log-utility of returns and avoid risk of ruin.  For our purposes, it can also be used as a stack-ranking method like \code{\link{InformationRatio}} to describe the \dQuote{edge} an investment would have over a random strategy or distribution.

These metrics and others such as \code{\link{SharpeRatio}}, \code{\link{SortinoRatio}}, \code{\link{UpsidePotentialRatio}}, Spearman rank correlation (see \code{\link[Hmisc]{rcorr}}), etc., are all methods of rank-ordering relative performance. \cite{Alexander and Dimitriu (2004) in \dQuote{The Art of Investing in Hedge Funds}} show that relative rankings across multiple pricing methodologies may be positively correlated with each other and with expected returns.  This is quite an important finding because it shows that multiple methods of predicting returns and risk which have underlying measures and factors that are not directly correlated to another measure or factor will still produce widely similar quantile rankings, so that the \dQuote{buckets} of target instruments will have significant overlap.  This observation specifically supports the point made early in this document regarding \dQuote{accretion of the evidence} for a positive or negative investment decision.
}

\section{Style Analysis}{
Style analysis is one way to help determine a fund's exposures to the changes in returns of major asset classes or other factors.  \kbd{PerformanceAnalytics} previously had a few functions that calculate style weights using an asset class style model as described in detail in Sharpe (1992).

These functions have been moved to \kbd{R-Forge} in package \kbd{FactorAnalytics} as part of a collaboration with Eric Zivot at the University of Washington. The functions combine to calculate effective style weights and display the results in a bar chart.  \code{chart.Style} calculates and displays style weights calculated over a single period. \code{chart.RollingStyle} calculates and displays those weights in rolling windows through time.  \code{style.fit} manages the calculation of the weights by method, and \code{style.QPfit} calculates the specific constraint case that requires quadratic programming.  [note: these functions do not currently appear in the development codebase, but should reappear as a supported method at some point]

There is a significant amount of academic literature on identifying and attributing sources of risk or returns.  Much of it falls into the field of \dQuote{factor analysis} where \dQuote{risk factors} are used to retrospectively explain sources of risk, and through regression and other analytical methods \emph{predict} future period returns and risk based on factor drivers.  These are well covered in chapters on factor analysis in \cite{Zivot and Wang(2006)} and also in the \R functions \code{\link{factanal}} for basic factor analysis and \code{\link{princomp}} for Principal Component Analysis.  The authors feel that financial engineers and analysts would benefit from some wrapping of this functionality focused on finance, but the capabilities already available from the base functions are quite powerful.  We are hopeful that our new collaboration with Prof. Zivot will provide additional functionality in the near future.
}

\section{Risk Analysis}{
Many methods have been proposed to measure, monitor, and control the risks of a diversified portfolio. Perhaps a few definitions are in order on how different risks are generally classified. \emph{Market Risk} is the risk to the portfolio from a decline in the market price of instruments in the portfolio.  \emph{Liquidity Risk} is the risk that the holder of an instrument will find that a position is illiquid, and will incur extra costs in unwinding the position resulting in a less favorable price for the instrument. In  extreme cases of liquidity risk, the seller may be unable to find a buyer for the instrument at all, making the value unknowable or zero.  \emph{Credit Risk} encompasses \emph{Default Risk}, or the risk that promised payments on a loan or bond will not be made, or that a convertible instrument will not be converted in a timely manner or at all.  There are also \emph{Counterparty Risks} in over the counter markets, such as those for complex derivatives.  Tools have evolved to measure all these different components of risk.  Processes must be put into place inside a firm to monitor the changing risks in a portfolio, and to control the magnitude of risks.  For an extensive treatment of these topics, see Litterman, Gumerlock, et. al.(1998).  For our purposes, \kbd{PerformanceAnalytics} tends to focus on market and liquidity risk.

The simplest risk measure in common use is volatility, usually modeled quantitatively with a univariate standard deviation on a portfolio.  See \code{\link[stats]{sd}}.  Volatility or Standard Deviation is an appropriate risk measure when the distribution of returns is normal or resembles a random walk, and may be annualized using \code{\link{sd.annualized}}, or the equivalent function \code{\link{sd.multiperiod}} for scaling to an arbitrary number of periods.  Many assets, including hedge funds, commodities, options, and even most common stocks over a sufficiently long period, do not follow a normal distribution.  For such common but non-normally distributed assets, a more sophisticated approach than standard deviation/volatility is required to adequately model the risk.

Markowitz, in his Nobel acceptance speech and in several papers, proposed that \code{\link{SemiVariance}} would be a better measure of risk than variance.  See \cite{Zin, Markowitz, Zhao (2006)}.  This measure is also called \code{\link{SemiDeviation}}.  The more general case is \code{\link{DownsideDeviation}}, as proposed by \cite{Sortino and Price(1994)}, where the minimum acceptable return (MAR) is a parameter to the function.  It is interesting to note that variance and mean return can produce a smoothly elliptical efficient frontier for portfolio optimization using \code{\link[quadprog]{solve.QP}} or \code{\link[tseries]{portfolio.optim}} or \kbd{fPortfolio}. Use of semivariance or many other risk measures will not necessarily create a smooth ellipse, causing significant additional difficulties for the portfolio manager trying to build an optimal portfolio.  We'll leave a more complete treatment and implementation of portfolio optimization techniques for another time.

Another very widely used downside risk measures is analysis of drawdowns, or loss from peak value achieved. The simplest method is to check the \code{\link{maxDrawdown}}, as this will tell you the worst cumulative loss ever sustained by the asset.  If you want to look at all the drawdowns, you can \code{\link{findDrawdowns}} and \code{\link{sortDrawdowns}} in order from worst/major to smallest/minor. The \code{\link{UpDownRatios}} function will give you some insight into the impacts of the skewness and kurtosis of the returns, and letting you know how length and magnitude of up or down moves compare to each other.  You can also plot drawdowns with \code{\link{chart.Drawdown}}.

One of the most commonly used and cited measures of the risk/reward tradeoff of an investment or portfolio is the \code{\link{SharpeRatio}}, which measures return over standard deviation.  If you are comparing multiple assets using Sharpe, you should use \code{\link{SharpeRatio.annualized}}. It is important to note that William Sharpe now recommends \code{\link{InformationRatio}} preferentially to the original Sharpe Ratio. The \code{\link{SortinoRatio}} uses mean return over \code{\link{DownsideDeviation}} below the MAR as the risk measure to produce a similar ratio that is more sensitive to downside risk. Sortino later enhanced his ideas to use upside returns for the numerator and \code{\link{DownsideDeviation}} as the denominator in \code{\link{UpsidePotentialRatio}}. \cite{Favre and Galeano(2002)} propose using the ratio of expected excess return over the Cornish-Fisher \code{\link{VaR}} to produce \code{\link{SharpeRatio.modified}}. \code{\link{TreynorRatio}} is also similar to the Sharpe Ratio, except it uses \code{\link{CAPM.beta}} in place of the volatility measure to produce the ratio of the investment's excess return over the beta.

One of the newer statistical methods developed for analyzing the risk of financial instruments is \code{\link{Omega}}.  Omega analytically constructs a cumulative distribution function, in a manner similar to \code{\link{chart.QQPlot}}, but then extracts additional information from the location and slope of the derived function at the point indicated by the risk quantile that the researcher is interested in.  Omega seeks to combine a large amount of data about the shape, magnitude, and slope of the distribution into one method.  The academic literature is still exploring the best manner to use Omega in a risk measurement and control process, or in portfolio construction.

Any risk measure should be viewed with suspicion if there are not a large number of historical observations of returns for the asset in question available.  Depending on the measure, the number of observations required will vary greatly from a statistical standpoint.  As a heuristic rule, ideally you will have data available on how the instrument performed through several economic cycles and shocks.  When such a long history is not available, the investor or researcher has several options.  A full discussion of the various approaches is beyond the scope of this introduction, so we will merely touch on several areas that an interested party may wish to explore in additional detail. Examining the returns of assets with a similar style, industry, or asset class to which the asset in question is highly correlated and shares other characteristics can be quite informative.  Factor analysis may be used to uncover specific risk factors where transparency is not available. Various resampling (see \code{\link[tseries]{tsbootstrap}}) and simulation methods are available in \R to construct an artificially long distribution for testing.  If you use a method such as Monte Carlo simulation or the bootstrap, it is often valuable to use \code{\link{chart.Boxplot}} to visualize the different estimates of the risk measure produced by the simulation, to see how small (or wide) a range the estimates cover, and thus gain a level of confidence with the results.  Proceed with extreme caution when your historical data is lacking.  Problems with lack of historical data are a major reason why many institutional investors will not invest in an alternative asset without several years of historical return data available.
}

\section{Value at Risk - VaR}{
\emph{Traditional mean-VaR}:
In the early 90's, academic literature started referring to \dQuote{value at risk}, typically written as VaR. Take care to capitalize VaR in the commonly accepted manner, to avoid confusion with var (variance) and VAR (vector auto-regression).  With a sufficiently large data set, you may choose to use a non-parametric VaR estimation method using the historical distribution and the probability quantile of the distribution calculated using \code{\link{qnorm}}. The negative return at the correct quantile (usually 95\% or 99\%), is the non-parametric VaR estimate.  J.P. Morgan's RiskMetrics parametric mean-VaR was published in 1994 and this methodology for estimating parametric mean-VaR has become what people are generally referring to as \dQuote{VaR} and what we have implemented as \code{\link{VaR}} with \code{method="historical"}.  See \cite{Return to RiskMetrics: Evolution of a Standard} at \url{http://www.riskmetrics.com/r2rovv.html}. Parametric traditional VaR does a better job of accounting for the tails of the distribution by more precisely estimating the tails below the risk quantile.  It is still insufficient if the assets have a distribution that varies widely from normality.  That is available in \code{\link{VaR}} with \code{method="gaussian"}.

The \R package VaR, now orphaned, contains methods for simulating and estimating lognormal \kbd{VaR.norm} and generalized Pareto \kbd{VaR.gpd} distributions to overcome some of the problems with nonparametric or parametric mean-VaR calculations on a limited sample size.  There is also a \kbd{VaR.backtest} function to apply simulation methods to create a more robust estimate of the potential distribution of losses.  The VaR package also provides plots for its functions.  We will attempt to incoporate this orphaned functionality in PerformanceAnalytics in an upcoming release.

\emph{Modified Cornish-Fisher VaR}:
The limitations of traditional mean-VaR are all related to the use of a symmetrical distribution function.  Use of simulations, resampling, or Pareto distributions all help in making a more accurate prediction, but they are still flawed for assets with significantly non-normal (skewed and/or kurtotic) distributions. \cite{Huisman (1999)} and \cite{Favre and Galleano (2002)} propose to overcome this extensively documented failing of traditional VaR by directly incorporating the higher moments of the return distribution into the VaR calculation.  

This new VaR measure incorporates skewness and kurtosis via an analytical estimation using a Cornish-Fisher (special case of a Taylor) expansion. The resulting measure is referred to variously as \dQuote{Cornish-Fisher VaR} or \dQuote{Modified VaR}.  We provide this measure in function \code{\link{VaR}} with \code{method="modified"}.  Modified VaR produces the same results as traditional mean-VaR when the return distribution is normal, so it may be used as a direct replacement.  Many papers in the finance literature have reached the conclusion that Modified VaR is a superior measure, and may be substituted in any case where mean-VaR would previously have been used.  

\emph{Conditional VaR and Expected Shortfall}:
We have implemented Conditional Value at Risk, also called Expected Shortfall (not to be confused with shortfall probability, which is much less useful), in function \code{\link{ES}}.  Expected Shortfall attempts to measure the magnitude of the average loss exceeding the traditional mean-VaR. Expected Shortfall has proven to be a reasonable risk predictor for many asset classes.  We have provided traditional historical, Gaussian and modified Cornish-Fisher measures of Expected Shortfall by using \code{method="historical"}, \code{method="gaussian"} or \code{method="modified"}. See \cite{Uryasev(2000)} and \cite{Sherer and Martin(2005)} for more information on Conditional Value at Risk and Expected Shortfall.  Please note that your milage will vary; expect that values obtained from the normal distribution may differ radically from the real situation, depending on the assets under analysis.

\emph{Multivariate extensions to risk measures}:
We have extened all moments calculations to work in a multivariate portfolio context.  In a portfolio context the multivariate moments are generally to be preferred to their univariate counterparts, so that all information is available to subsequent calculations.  Both the \code{\link{VaR}} and \code{\link{ES}} functions allow calculation of metrics in a portfolio context when \code{weights} and a \code{portfolio_method} are passed into the function call.

\emph{Marginal, Incremental, and Component VaR}:
Marginal VaR is the difference between the VaR of the portfolio without the asset in question and the entire portfolio.  The \code{\link{VaR}} function calculates Marginal VaR for all instruments in the portfolio if you set \code{method="marginal"}. Marginal VaR as provided here may use traditional mean-VaR or Modified VaR for the calculation. Per \cite{Artzner,et.al.(1997)} properties of a coherent risk measure include subadditivity (risks of the portfolio should not exceed the sum of the risks of individual components) as a significantly desirable trait.  VaR measures, including Marginal VaR, on individual components of a portfolio are \emph{not} subadditive. 

Clearly, a general subadditive risk measure for downside risk is required.  In Incremental or Component VaR, the Component VaR value for each element of the portfolio will sum to the total VaR of the portfolio.  Several EDHEC papers suggest using Modified VaR instead of mean-VaR in the Incremental and Component VaR calculation.  We have succeeded in implementing Component VaR and ES calculations that use Modified Cornish-Fisher VaR, historical decomposition, and kernel estimators.  You may access these with \code{\link{VaR}} or \code{\link{ES}} by setting the appropriate \code{portfolio_method} and \code{method} arguments.

The \code{\link{chart.VaRSensitivity}} function creates a chart of Value-at-Risk or Expected Shortfall estimates by confidence interval for multiple methods. Useful for comparing a calculated VaR or ES method to the historical VaR or ES, it may also be used to visually examine whether the VaR method \dQuote{breaks down} or gives nonsense results at a certain threshold.

Which VaR measure to use will depend greatly on the portfolio and instruments being analyzed.  If there is any generalization to be made on VaR measures, we agree with \cite{Bali and Gokcan(2004)} who conclude that \dQuote{the VaR estimations based on the generalized Pareto distribution and the Cornish-Fisher approximation perform best}.
}

\section{Moments and Co-moments}{
Analysis of financial time series often involves evaluating their mathematical moments.  While \code{\link{var}} and \code{\link{cov}} for variance has always been available, as well as  \code{\link{skewness}} and \code{\link{kurtosis}} (which we have extended to make multivariate and multi-column aware), a larger suite of multivariate moments calculations was not available in \R.  We have now implemented multivariate moments and co-moments and their beta or systematic co-moments in \kbd{PerformanceAnalytics}.

\cite{Ranaldo and Favre (2005)} define coskewness and cokurtosis as the skewness and kurtosis of a given asset analysed with the skewness and kurtosis of the reference asset or portfolio.  The co-moments are useful for measuring the marginal contribution of each asset to the portfolio's resulting risk.  As such, co-moments of an asset return distribution should be useful as inputs for portfolio optimization in addition to the covariance matrix.  Functions include \code{\link{CoVariance}}, \code{\link{CoSkewness}}, \code{\link{CoKurtosis}}.

Measuring the co-moments should be useful for evaluating whether or not an asset is likely to provide diversification potential to a portfolio.  But the co-moments do not allow the marginal impact of an asset on a portfolio to be directly measured.  Instead, \cite{Martellini and Zieman (2007)} develop a framework that assesses the potential diversification of an asset relative to a portfolio. They use higher moment betas to estimate how much portfolio risk will be impacted by adding an asset.  

Higher moment betas are defined as proportional to the derivative of the covariance, coskewness and cokurtosis of the second, third and fourth portfolio moment with respect to the portfolio weights.  A beta that is less than 1 indicates that adding the new asset should reduce the resulting portfolio's volatility and kurtosis, and to an increase in skewness. More specifically, the lower the beta the higher the diversification effect, not only in terms of normal risk (i.e. volatility) but also the risk of assymetry (skewness) and extreme events (kurtosis).  See the functions for \code{\link{BetaCoVariance}}, \code{\link{BetaCoSkewness}}, and \code{\link{BetaCoKurtosis}}.
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

    \code{\link{table.Stats}} \tab Basic statistics and stylized facts \cr
    \code{\link{table.TrailingPeriods}} \tab Statistics and stylized facts compared over different trailing periods \cr
    \code{\link{table.AnnualizedReturns}} \tab Annualized return, standard deviation, and Sharpe ratio \cr
    \code{\link{table.CalendarReturns}} \tab Monthly and calendar year return table \cr
    \code{\link{table.CAPM}} \tab CAPM-related measures \cr
    \code{\link{table.Correlation}} \tab Comparison of correlalations and significance statistics \cr
    \code{\link{table.DownsideRisk}} \tab Downside risk metrics and statistics \cr
    \code{\link{table.Drawdowns}} \tab Ordered list of drawdowns and when they occurred \cr
    \code{\link{table.Autocorrelation}} \tab The first six autocorrelation coefficients and significance \cr
    \code{\link{table.HigherMoments}} \tab Higher co-moments and beta co-moments \cr
    \code{\link{table.Arbitrary}} \tab Combines a function list into a table \cr
  }
}

\section{Charts and Graphs}{
Graphs and charts can also help to organize the information visually. Our goal in creating these charts was to simplify the process of creating well-formatted charts that are used often in performance analysis, and to create high-quality graphics that may be used in documents for consumption by non-analysts or researchers. \R's graphics capabilities are substantial, but the simplicity of the output of \R default graphics functions such as \code{\link[graphics]{plot}} does not always compare well against graphics delivered with commercial asset or performance analysis from places such as MorningStar or PerTrac.

The cumulative returns or wealth index is usually the first thing displayed, even though neither conveys much information.  See \code{\link{chart.CumReturns}}.  Individual period returns may be helpful for identifying problematic periods, such as in \code{\link{chart.Bar}}.  Risk measures can be helpful when overlaid on the period returns, to display the bounds at which losses may be expected.  See \code{\link{chart.BarVaR}} and the prior section on Risk Analysis.  More information can be conveyed when such charts are displayed together, as in \code{\link{charts.PerformanceSummary}}, which combines the performance data with detail on downside risk (see \code{\link{chart.Drawdown}}).

\code{\link{chart.RelativePerformance}} can plot the relative performance through time of two assets.  This plot displays the ratio of the cumulative performance at each point in time and makes periods of under- or out-performance easy to see.  The value of the chart is less important than the slope of the line.  If the slope is positive, the first asset is outperforming the second, and vice verse.  Affectionately known as the Canto chart, it was used effectively in Canto (2006).

Two-dimensional charts can also be useful while remaining easy to understand.  \code{\link{chart.Scatter}} is a utility scatter chart with some additional attributes that are used in \code{\link{chart.RiskReturnScatter}}.  Overlaying Sharpe ratio lines or boxplots helps to add information about relative performance along those dimensions. 

For distributional analysis, a few graphics may be useful.  \code{\link{chart.Boxplot}} is an example of a graphic that is difficult to create in Excel and is under-utilized as a result.  A boxplot of returns is, however, a very useful way to instantly observe the shape of large collections of asset returns in a manner that makes them easy to compare to one another.  \code{\link{chart.Histogram}} and \code{\link{chart.QQPlot}} are two charts originally found elsewhere and now substantially expanded in \kbd{PerformanceAnalytics}.

Rolling performance is typically used as a way to assess stability of a return stream.  Although perhaps it doesn't get much credence in the financial literature as it derives from work in digital signal processing, many practitioners find it a useful way to examine and segment performance and risk periods.  See \code{\link{chart.RollingPerformance}}, which is a way to display different metrics over rolling time periods.  \code{\link{chart.RollingMean}} is a specific example of a rolling mean and standard error bands.  A group of related metrics is offered in \code{\link{charts.RollingPerformance}}.  These charts use utility functions such as \code{\link[zoo]{rollapply}}.

\code{\link{chart.SnailTrail}} is a scatter chart that shows how rolling calculations of annualized return and annualized standard deviation have proceeded through time where the color of lines and dots on the chart diminishes with respect to time. \code{\link{chart.RollingCorrelation}} shows how correlations change over rolling periods.  \code{\link{chart.RollingRegression}} displays the coefficients of a linear model fitted over rolling periods.  A group of charts in \code{\link{charts.RollingRegression}} displays alpha, beta, and R-squared estimates in three aligned charts in a single device.

\code{\link{chart.StackedBar}} creates a stacked column chart with time on the horizontal axis and values in categories.  This kind of chart is commonly used for showing portfolio 'weights' through time, although the function will plot any values by category. 

We have been greatly inspired by other peoples' work, some of which is on display at \url{http://addictedtor.free.fr/}.  Particular inspiration came from Dirk Eddelbuettel and John Bollinger for their work at \url{http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=65}. Those interested in price charting in \R should also look at the \code{quantmod} package.
}

\section{Wrapper and Utility Functions}{
\R is a very powerful environment for manipulating data.  It can also be quite confusing to a user more accustomed to Excel or even MatLAB.  As such, we have written some wrapper functions that may aid you in coercing data into the correct forms or finding data that you need to use regularly.  To simplify the management of multiple-source data stored in \R in multiple data formats, we have provided \code{\link{checkData}}.  This function will attempt to coerce data in and out of \R's multitude of mostly fungible data classes into the class required for a particular analysis.  The data-coercion function has been hidden inside the functions here, but it may also save you time and trouble in your own code and functions as well.

\R's built-in \code{\link{apply}} function in enormously powerful, but is can be tricky to use with timeseries data, so we have provided wrapper functions to \code{\link{apply.fromstart}} and \code{\link{apply.rolling}} to make handling of \dQuote{from inception} and \dQuote{rolling window} calculations easier.
}

\section{Further Work}{
We have attempted to standardize function parameters and variable names, but more work exists to be done here.

Any comments, suggestions, or code patches are invited.

If you've implemented anything that you think would be generally useful to include, please consider donating it for inclusion in a later version of this package.
}

\author{
Brian G. Peterson \cr
Peter Carl \cr

Maintainer: Brian G. Peterson \email{brian@braverock.com}
}

\references{
Amenc, N. and Le Sourd, V. \emph{Portfolio Theory and Performance Analysis}. Wiley. 2003. \cr 

Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. \cr

Canto, V. \emph{Understanding Asset Allocation}. FT Prentice Hall. 2006. \cr

Lhabitant, F. \emph{Hedge Funds: Quantitative Insights}. Wiley. 2004. \cr

Litterman, R., Gumerlock R., et. al. \emph{The Practice of Risk Management: Implementing Processes for Managing Firm-Wide Market Risk}. Euromoney. 1998. \cr

Martellini, Lionel, and Volker Ziemann. \emph{Improved Forecasts of Higher-Order Comoments and Implications for Portfolio Selection.} EDHEC Risk and Asset Management Research Centre working paper. 2007. \cr

Ranaldo, Angelo, and Laurent Favre Sr. \emph{How to Price Hedge Funds: From Two- to Four-Moment CAPM.} SSRN eLibrary. 2005.

Murrel, P. \emph{R Graphics}. Chapman and Hall. 2006.  \cr

Ruppert, D. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr

Scherer, B. and Martin, D. \emph{Modern Portfolio Optimization}. Springer. 2005. \cr

Shumway, R. and Stoffer, D. \emph{Time Series Analysis and it's Applications, with R examples}, Springer, 2006. \cr

Tsay, R. \emph{Analysis of Financial Time Series}. Wiley. 2001. \cr

Zin, Markowitz, Zhao A Note on Semivariance. Mathematical Finance, Vol. 16, No. 1, pp. 53-61, January 2006 

Zivot, E. and Wang, Z. \emph{Modeling Financial Time Series with S-Plus: second edition}. Springer. 2006. \cr
}

\seealso{
    CRAN task view on Empirical Finance \cr \url{http://cran.r-project.org/src/contrib/Views/Econometrics.html}

    Grant Farnsworth's Econometrics in R \cr \url{http://cran.r-project.org/doc/contrib/Farnsworth-EconometricsInR.pdf}

    Collection of R charts and graphs \cr \url{http://addictedtor.free.fr/graphiques/}

}

\section{Acknowledgments}{
Data series \code{\link{edhec}} used in \kbd{PerformanceAnalytics} and related publications with the kind permission of the EDHEC Risk and Asset Management Research Center. \cr
\url{http://www.edhec-risk.com/indexes/pure_style}

Kris Boudt was instrumental in our research on component risk for portfolios with non-normal distributions, and is responsible for much of the code for multivariate moments and co-moments.

Jeff Ryan and Josh Ulrich are active participants in the R finance community and created \code{\link[xts]{xts}}, upon which much of PerformanceAnalytics depends.

Prototypes of the drawdowns functionality were provided by Sankalp Upadhyay, and modified with permission. Stephan Albrecht provided detailed feedback on the Getmansky/Lo Smoothing Index.  Diethelm Wuertz provided prototypes of modified VaR and skewness and kurtosis functions (and is of course the maintainer of the RMetrics suite of pricing and optimization functions).  He also contributed prototypes for many other functions from Bacon's book that were incorporated into PerformanceAnalytics by Matthieu Lestel.  Any errors are, of course, our own.

Thanks to Joe Wayne Byers and Dirk Eddelbuettel for comments on early versions of these functions, and to Khanh Nguyen, Tobias Verbeke, H. Felix Wittmann, and Ryan Sheftel for careful testing and detailed problem reports.

Thanks also to our Google Summer of Code students through the years for their contributions.  Significant contributions from GSOC students to this package have come from Matthieu Lestel and Andrii Babii so far.  We expect to eventually incorporate contributions from Pulkit Mehrotra and Shubhankit Mohan, who worked with us during the summer of 2013.

Thanks to the R-SIG-Finance community without whom this package would not be possible.  We are indebted to the R-SIG-Finance community for many helpful suggestions, bugfixes, and requests.
}


\keyword{ package }
