\name{chart.RiskReturnScatter}
\alias{chart.RiskReturnScatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.RiskReturnScatter(R, rf = 0, main = "Annualized Return and Risk", add.names = TRUE, xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", add.sharpe = c(1, 2, 3), add.boxplots = FALSE, colorset = 1, symbolset = 1, darken = FALSE, legend.loc = NULL, xlim = NULL, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{add.names}{ ~~Describe \code{add.names} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{add.sharpe}{ ~~Describe \code{add.sharpe} here~~ }
  \item{add.boxplots}{ ~~Describe \code{add.boxplots} here~~ }
  \item{colorset}{ color palette to use, set by default to rational choices }
  \item{symbolset}{ ~~Describe \code{symbolset} here~~ }
  \item{darken}{ ~~Describe \code{darken} here~~ }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
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
function (R, rf = 0, main = "Annualized Return and Risk", add.names = TRUE, xlab = "Annualized Risk", ylab = "Annualized Return", method = "calc", add.sharpe = c(1,2,3), add.boxplots = FALSE, colorset = 1, symbolset = 1, darken = FALSE , legend.loc = NULL, xlim = NULL, ylim = NULL, ...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A wrapper to create a scatter chart of annualized returns versus
    # annualized risk (standard deviation) for comparing manager performance.
    # Also puts a box plot into the margins to help identify the relative
    # performance quartile.

    # Inputs:
    # R = usually a set of monthly return, but can also be a pre-calculated
    #   return and risk measure (set method = "nocalc", see below).  If the input
    #   is a set of monthly performance returns, this function will calculate
    #   the appropriate return and risk summary according to the method flag.
    # method = If method is set to "nocalc" then we assume that R is a column of
    #   return and a column of risk (e.g., annualized returns, annualized risk),
    #   in that order.  Other method cases can be set for different risk/return
    #   calculations.
    # rf = this is the risk free rate.  Remember to set this to the same
    #   periodicity as the data being passed in.
    # add.sharpe = this draws a Sharpe ratio line that indicates Sharpe ratio
    #   levels.  Lines are drawn with a y-intercept of the risk free rate and
    #   the slope of the appropriate Sharpe ratio level.  Lines should be
    #   removed where not appropriate (e.g., sharpe.ratio = NULL).
    # add.names = plots the row name with the data point.  Can be removed with
    #   by setting it to NULL.
    # add.boxplots = adds a boxplot summary of the data on the axis.

    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    x = checkDataMatrix(R)

    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    # @todo: strip out basic elements to a scatter plot wrapper
    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    # @todo: add flags to use other risk measures

    if(method == "calc"){
        # Assume we're passed in a series of monthly returns.  First, we'll
        # annualized returns and risk
        comparison = t(table.AnnualizedReturns(x, rf = rf))

        returns = comparison[,1]
        risk = comparison[,2]
        #sharpe = comparison[,3]
    } else {
        # We have to make an assumption about the input here
        returns = x[,1]
        risk = x[,2]
    }

    # Set the charts to show the origin
    if(is.null(xlim[1]))
        xlim = c(0, max(risk) + 0.02)
    if(is.null(ylim[1]))
        ylim = c(0, max(returns) + 0.02)

    if(add.boxplots){
        original.layout <- par()
        layout( matrix( c(2,1,0,3), 2, 2, byrow=T ),
                c(1,6), c(4,1),
            )
        par(mar=c(1,1,5,2))
    }

    # Draw the principal scatterplot
    plot(returns ~ risk,
        xlab='', ylab='',
        las = 1, xlim=xlim, ylim=ylim, cex.axis = .8, col = colorset, pch = symbolset, ...)
#     abline(v = 0, col = elementcolor)
#     abline(h = 0, col = elementcolor)

    if(!add.boxplots){
        title(ylab = ylab)
        title(xlab = xlab)
    }

    # Add Sharpe ratio lines
    # @todo: Drawing Sharpe ratio lines currently throws warnings; change test statement
    if(!is.na(add.sharpe[1])) {
        for(line in add.sharpe) {
        abline(a=(rf*12),b=add.sharpe[line],col="gray",lty=2)
        }
    }

    # Label the data points
    if(add.names)
        text(x = risk,y = returns, labels = row.names(comparison), adj = -0.1, cex = 0.8, col = colorset)

    # Add a rug so that data points are easier to identify
    rug(side=1, risk, col = elementcolor)
    rug(side=2, returns, col = elementcolor)

    title(main = main)

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = 0.8, border.col = elementcolor, pch = symbolset, bg = "white", legend = columnnames)
    }

    #title(sub='From Inception', line=1)
    box()

    if(add.boxplots){
        # Draw the Y-axis histogram
        par(mar=c(1,2,5,1))
        boxplot(returns, axes = FALSE, ylim = ylim)
        title(ylab = ylab, line = 0)

        # Draw the X-axis histogram
        par(mar=c(5,1,1,2))
        boxplot(risk, horizontal = TRUE, axes = FALSE, ylim=xlim)
        title(xlab = xlab, line = 1)

        #par(original.layout)
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
