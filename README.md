# PerformanceAnalytics: Econometric tools for performance and risk analysis.

<!-- badges: start -->
[![R-CMD-check](https://github.com/braverock/PerformanceAnalytics/workflows/R-CMD-check/badge.svg)](https://github.com/braverock/PerformanceAnalytics/actions)
[![Travis Build
Status](https://travis-ci.org/braverock/PerformanceAnalytics.svg?branch=master)](https://travis-ci.org/braverock/PerformanceAnalytics)
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/PerformanceAnalytics)](https://cran.r-project.org/package=PerformanceAnalytics)
<!-- badges: end -->

`PerformanceAnalytics` provides an R package of econometric functions
for performance and risk analysis of financial instruments or portfolios.
This package aims to aid practitioners and researchers in using the latest research for
analysis of both normally and non-normally distributed return streams.

We created this package to include functionality that has been appearing in the academic literature
on performance analysis and risk over the past several years, but had no functional equivalent in R.
In doing so, we also found it valuable to have wrappers for some functionality
with good defaults and naming consistent with common usage in the finance literature.  

In general, this package requires return (rather than price) data.
Almost all of the functions will work with any periodicity,
from annual, monthly, daily, to even minutes and seconds, either regular or irregular.

The package documentation includes sections on

* Time Series Data
* Performance Analysis
* Style Analysis
* Risk Analysis
* Value at Risk - VaR
* Moments and Co-moments
* Robust Data Cleaning
* Summary Tabular Data
* Charts and Graphs
* Wrapper and Utility Functions
* Standard Errors for Risk and Performance Estimators

It also includes some thoughts on work yet to be done,
acknowledgments,
and pointers to other literature and resources in R useful for performance and risk analysis,

Some sample data is provided in the `managers` dataset.
It is an `xts` object that contains columns of monthly returns for six hypothetical asset managers (HAM1 through HAM6),
the EDHEC Long-Short Equity hedge fund index, the S&P 500 total returns,
and total return series for the US Treasury 10-year bond and 3-month bill.
Monthly returns for all series end in December 2006 and begin at different periods starting from January 1996.
That data set is used extensively in our examples and should serve as a model for formatting your data.
