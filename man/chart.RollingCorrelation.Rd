\name{chart.RollingCorrelation}
\alias{chart.RollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.RollingCorrelation(R, Rb, n = 12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), trim = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{Rb}{ ~~Describe \code{Rb} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{xaxis}{ ~~Describe \code{xaxis} here~~ }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, Rb, n=12, xaxis = TRUE, legend.loc = NULL, colorset = (1:12), trim = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of rolling performance metrics in a line chart

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # FUN: any function that can be evaluated using a single set of returns
    #   (e.g., rolling beta won't work, but Return.annualizeds will)

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)

    chart.TimeSeries(rollingCorrelation(x = x, y = y, n = n, trim = trim), xaxis = xaxis, col = colorset, legend.loc = legend.loc, ylim = c(-1,1), ...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
