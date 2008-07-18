\name{chart.RollingStyle}
\alias{chart.RollingStyle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate and display a chart of effective style weights through time }
\description{
  Calculates effective style weights over a rolling window and displays the results in a stacked bar chart
}
\usage{
chart.RollingStyle(R.fund, R.style, method = c("constrained","unconstrained","normalized"), leverage = FALSE, width = 12, main = paste(colnames(R.fund)[1]," Rolling ", width ,"-Month Style Weights", sep=""), space = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R.fund}{ matrix, data frame, or zoo object with fund returns to be analyzed }
  \item{R.style}{ matrix, data frame, or zoo object with style index returns.  Data object must be of the same length and time-aligned with R.fund }
  \item{method}{ specify the method of calculation of style weights as "constrained", "unconstrained", or "normalized".  For more information, see \link{\code{style.fit}} }
  \item{leverage}{ logical, defaults to 'FALSE'.  If 'TRUE', the calculation of weights assumes that leverage may be used.  For more information, see \link{\code{style.fit}} }
  \item{width}{ number of periods or window to apply rolling style analysis over }
  \item{main}{ set the chart title, same as in \code{\link{plot}} }
  \item{space}{ the amount of space (as a fraction of the average bar width) left before each bar, as in \code{\link{barplot}}. Default for this chart is 0. }
  \item{\dots}{ arguments to be passed to \code{\link{barplot}}. These can include further arguments (such as 'axes', 'asp' and 'main') and graphical parameters (see 'par') which are passed to 'plot.window()', 'title()' and 'axis'. }
}
\details{
This function calculates style weights from an asset class style model as described in detail in Sharpe (1992).  The use of quadratic programming to determine a fund's exposures to the changes in returns of major asset classes is usually refered to as "style analysis".

The "unconstrained" method implements a simple factor model for style analysis
\deqn{Ri = b.i1F1+b.i2F2+b.inFn +ei}{R_i = b_{i1}F_1+b_{i2}F_2+b_{in}F_n +e_i}
where \eqn{Ri}{R_i} represents the return on asset i, \eqn{Fj}{F_j} represents each factor, and \eqn{ei}{e_i} represents the "non-factor" component of the return on i.  This is simply a multiple regression analysis with fund returns as the dependent variable and asset class returns as the independent variables.  The resulting slope coefficients are then interpreted as the fund's historic exposures to asset class returns.  In this case, coefficients do not sum to 1.

The "normalized" method reports the results of a multiple regression analysis similar to the first, but with one constraint: the coefficients are required to add to 1.  Coefficients may be negative, indicating short exposures.

The "constrained" method includes the constraint that the coefficients sum to 1, but adds that the coefficients must lie between 0 and 1. These inequality constraints require a quadratic programming algorithm using \link{\code{solve.QP}} from the 'quadprog' package, and the implementation is discussed under \link{\code{style.QPfit}}.  If set to TRUE, "leverage" allows the sum of the coefficients to exceed 1. 

The function then provides a graphical summary of the results.  The underlying function, \link{\code{style.fit}}, provides the outputs of the analysis and more information about fit, including an R-squared value.

Styles identified in this analysis may be interpreted as an average of potentially changing exposures over the period covered.  This function is intended for examining the behavior of a manager's average exposures to asset classes over time, using a rolling-window analysis.

}
\value{
  Plots a column chart or stacked column chart of the resulting style weights to the current device.
}
\references{ 
Sharpe, W. Asset Allocation: Management Style and Performance Measurement Journal of Portfolio Management, 1992, 7-19.  See http://www.stanford.edu/~wfsharpe/art/sa/sa.htm
}
\author{ Peter Carl }
\note{ Unlike \code{\link{chart.Style}}, this function does align data sets and dates correctly between the two sets of inputs.
}
\seealso{ \code{\link{chart.Style}}, \code{\link{style.fit}}, \code{\link{style.QPfit}}, \code{\link{barplot}}, \code{\link{par}}  }
\examples{
data(edhec)
data(managers)
chart.RollingStyle(managers[,2,drop=F],edhec[,1:11], method="constrained", leverage=FALSE, width=36, cex.legend = .7, colorset=rainbow12equal, las=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ hplot }
