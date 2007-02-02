\name{chart.RelativePerformance}
\alias{chart.RelativePerformance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.RelativePerformance(R, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{Rb}{ ~~Describe \code{Rb} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{xaxis}{ ~~Describe \code{xaxis} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{ylog}{ ~~Describe \code{ylog} here~~ }
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
function (R, Rb, main = "Relative Performance", xaxis = TRUE, colorset = (1:12), legend.loc = NULL, ylog = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of relative returns through time

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # Rb: a matrix, data frame, or timeSeries of returns for a benchmark

    # Outputs:
    # A timeseries line chart of the calculated series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)
    y = checkDataMatrix(Rb)
    n = ncol(y)

    cumulativex = cumprod.column(1+x)
    cumulativey = cumprod.column(1+y)

    # Assumes that the first column is the one we want to compare to everything
    # in y.
    results = cumulativex[,1]/cumulativey[,1:n]

    # Removes the first color in the colorset to keep consistant with other graphics
    colorset = colorset[-1]

    chart.TimeSeries(results, xaxis = xaxis, main = main, legend.loc = legend.loc, col = colorset, ylog = ylog, ...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
