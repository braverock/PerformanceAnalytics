\name{chart.CumReturns}
\alias{chart.CumReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.CumReturns(R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{wealth.index}{ ~~Describe \code{wealth.index} here~~ }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
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
function (R, wealth.index = FALSE, legend.loc = NULL, colorset = (1:12), ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper function that cumulates the returns given and draws a line graph
    # of the results as a "wealth index".

    # Inputs:
    # R = a matrix, data frame, or timeSeries of returns
    # wealth.index = if true, shows the "value of $1", starting the cumulation
    #    of returns at 1 rather than zero
    # legend.loc: use this to locate the legend, e.g., "topright"
    # colorset: use the name of any of the palattes above

    # Outputs:
    # A timeseries line chart of the cumulative return series

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)

#     # Get dimensions and labels
#     columns = ncol(x)
#     rows = nrow(x)
#     columnnames = colnames(x)
#     rownames = rownames(x)

    # Calculate the cumulative return
    if(!wealth.index)
        Return.cumulative = cumprod.column(1+x) - 1
    else
        Return.cumulative = cumprod.column(1+x)

    # Chart the cumulative returns series
    chart.TimeSeries(Return.cumulative, col = colorset, legend.loc = legend.loc, ...)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
