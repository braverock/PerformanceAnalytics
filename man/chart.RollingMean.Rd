\name{chart.RollingMean}
\alias{chart.RollingMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ chart the rolling mean return }
\description{
A wrapper to create a rolling mean return chart with 95\% confidence bands.
}
\usage{
chart.RollingMean(R, n = 12, xaxis = TRUE, ylim = NULL, main = paste("Rolling ", n, "-Month Mean Return", sep = ""), trim = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{n}{ number of periods to apply rolling function over }
  \item{xaxis}{ if true, draws the x axis }
  \item{ylim}{ set the y-axis limit, same as in \code{\link{plot}} }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
A timeseries line charts of the rolling mean, with error bars
}
\references{ }
\author{ Peter Carl }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, n=12, xaxis = TRUE, ylim = NULL, main = paste("Rolling ",n,"-Month Mean Return",sep=""), trim = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a rolling mean return chart with 95\% confidence bands.

    # Inputs:
    # R: a matrix, data frame, or timeSeries, usually a set of monthly returns.
    #   The first column is assumed to be the returns of interest.

    # Outputs:
    # A timeseries line charts of the rolling mean, with error bars

    # FUNCTION:
    x = checkDataMatrix(R)
    x = as.data.frame(x[,1]) # @todo: handle multiple columns?
    ncols = ncol(x)
    rows = nrow(x)

    x.mean = rollingFunction(x, n = n, trim = trim, na.rm = FALSE, firstcolumn = 1, digits = 8, FUN = "mean")
    x.stdev = rollingFunction(x, n = n, trim = trim, na.rm = FALSE, firstcolumn = 1, digits = 8, FUN = "stdev")

    # @todo: allow user to set confidence interval
    # @todo: add chart for StdDev w confidence bands: x.stdev +- 2* x.stdev/sqrt(2*n)
    lower.band = x.mean - 2 * x.stdev/sqrt(n)
    upper.band = x.mean + 2 * x.stdev/sqrt(n)

    result = as.matrix(cbind(x.mean,lower.band,upper.band))

    # Set ylim correctly to allow for confidence bands
    if(is.null(ylim[1]))
        ylim = range(result[!is.na(result)])

    # The first row is the annualized returns
    chart.TimeSeries(result, ylim = ylim, main = main, xaxis = xaxis, ylab = "Mean Return", lty = c(1,2,2), colorset = c("black","darkgray","darkgray"), ...)

    #lines(1:rows, as.matrix(lower.band), col = "darkgray", lwd = 1, lty = "dashed", type = "l")
    #lines(1:rows, as.matrix(upper.band), col = "darkgray", lwd = 1, lty = "dashed", type = "l")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
