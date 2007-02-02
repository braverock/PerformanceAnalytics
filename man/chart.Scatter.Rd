\name{chart.Scatter}
\alias{chart.Scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.Scatter(x, y, reference.grid = TRUE, main = "Title", ylab = NULL, xlab = NULL, xlim = NA, ylim = NA, colorset = (1:12), symbolset = 1, darken = FALSE, legend.loc = NULL, ylog = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{reference.grid}{ ~~Describe \code{reference.grid} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{symbolset}{ ~~Describe \code{symbolset} here~~ }
  \item{darken}{ ~~Describe \code{darken} here~~ }
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
function (x, y, reference.grid = TRUE, main = "Title", ylab=NULL, xlab=NULL, xlim = NA, ylim = NA, colorset = (1:12), symbolset = 1, darken = FALSE , legend.loc = NULL, ylog = FALSE, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # Draws a scatter chart.  This is another chart "primitive", since it
    # only contains a set of sensible defaults.  This function is
    # intended to be used in a wrapper that is written for a particular purpose.
    # This is just a handy way to standardize the formatting of multiple charts.

    # Inputs:
    # x and y = assumes that data is a regular time series, not irregular.  Can take
    # any type of object, whether a matrix, data frame, or timeSeries.
    # legend.loc = use this to locate the legend, e.g., "topright"
    # colorset = use the name of any of the palattes above
    # reference.grid = if true, draws a grid aligned with the points on the
    #    x and y axes.
    # darken = if true, draws the chart elements in "darkgray" rather than
    #    "gray".  Makes it easier to print for some printers.


    # All other inputs are the same as "plot" and are principally included
    # so that some sensible defaults could be set.

    # Output:
    # Draws a timeseries graph of type "line" with some sensible defaults.

    # FUNCTION:
    x = checkDataVector(x)
    y = checkDataVector(y)

    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen


    plot(x~y, main = main, pch = symbolset, ylog = ylog, ...)

    if(reference.grid)
        grid(col = elementcolor)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
