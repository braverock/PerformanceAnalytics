\name{chart.Histogram}
\alias{chart.Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.Histogram(R, rf = 0, main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", add.density = TRUE, add.fit = TRUE, density.col = "black", fit.col = "darkgray", colorset = "gray", lwd = 2, xlim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ set the chart title, same as in \code{plot} }
  \item{add.names}{ ~~Describe \code{add.names} here~~ }
  \item{xlab}{ set the x-axis label, as in \code{\link{plot}} }
  \item{ylab}{ set the y-axis label, as in \code{\link{plot}} }
  \item{border.col}{ ~~Describe \code{border.col} here~~ }
  \item{add.density}{ ~~Describe \code{add.density} here~~ }
  \item{add.fit}{ ~~Describe \code{add.fit} here~~ }
  \item{density.col}{ ~~Describe \code{density.col} here~~ }
  \item{fit.col}{ ~~Describe \code{fit.col} here~~ }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{xlim}{ set the x-axis limit, same as in \code{\link{plot}} }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
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
function(R, rf = 0, main = NULL, add.names = TRUE, xlab = "Returns", ylab = "Frequency", border.col = "white", add.density = TRUE, add.fit = TRUE, density.col = "black", fit.col = "darkgray", colorset = "gray", lwd = 2, xlim = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # Create a histogram of returns, with optional curve fits for density
    # and normal

    # Inputs:
    # R = usually a set of monthly return

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    y = checkDataMatrix(R)
    x = checkDataVector(y[,1])

    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)

    if(is.null(main)){
        main = columnnames[1]
    }

    xlim = c(qnorm(0.001, mean(x), stdev(x)), qnorm(0.999, mean(x), stdev(x)))

    hist(x = x, probability = TRUE, xlim = xlim, col = colorset, border = border.col, xlab = xlab, main = main, breaks = "FD", ...)

    # Show Density Estimate:
    if(add.density){
        den = density(x)
        lines(den, col = density.col, lwd = lwd)
        #plot(den, xlab = xlab, ylab = ylab, xlim = xlim, lty = 3, lwd = 2, main = "Manager Returns", col = density.col)
    }

    # Add Normal Fit:
    if(add.fit){
        s = seq(xlim[1], xlim[2], length = 500)
        lines(s, dnorm(s, mean(x), stdev(x)), col = fit.col, lwd = lwd)
        # @todo: add other fits
        #lines(s, dcauchy(s,mean(x), stdev(x)))
    }

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
