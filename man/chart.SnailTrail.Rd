\name{chart.SnailTrail}
\alias{chart.SnailTrail}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.SnailTrail(R, rf = 0, main = "Annualized Return and Risk", add.labels = c("all", "lastonly", "firstandlast", "none"), xlab = "Annualized Risk", ylab = "Annualized Return", add.sharpe = c(1, 2, 3), colorset = NULL, symbolset = 16, darken = FALSE, legend.loc = NULL, xlim = NULL, ylim = NULL, width = 12, stepsize = 12, lty = 1, lwd = 2, first = TRUE, cex.axis = 0.8, cex.main = 1, cex.lab = 0.9, cex.text = 0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{rf}{ ~~Describe \code{rf} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{add.labels}{ ~~Describe \code{add.labels} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{add.sharpe}{ ~~Describe \code{add.sharpe} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{symbolset}{ ~~Describe \code{symbolset} here~~ }
  \item{darken}{ ~~Describe \code{darken} here~~ }
  \item{legend.loc}{ ~~Describe \code{legend.loc} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{stepsize}{ ~~Describe \code{stepsize} here~~ }
  \item{lty}{ ~~Describe \code{lty} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{first}{ ~~Describe \code{first} here~~ }
  \item{cex.axis}{ ~~Describe \code{cex.axis} here~~ }
  \item{cex.main}{ ~~Describe \code{cex.main} here~~ }
  \item{cex.lab}{ ~~Describe \code{cex.lab} here~~ }
  \item{cex.text}{ ~~Describe \code{cex.text} here~~ }
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
function (R, rf = 0, main = "Annualized Return and Risk", add.labels = c("all", "lastonly", "firstandlast", "none"), xlab = "Annualized Risk", ylab = "Annualized Return", add.sharpe = c(1,2,3), colorset = NULL, symbolset = 16, darken = FALSE , legend.loc = NULL, xlim = NULL, ylim = NULL, width = 12, stepsize = 12, lty=1, lwd=2, first = TRUE, cex.axis=0.8, cex.main = 1, cex.lab = .9, cex.text = 0.8,...)
{ # @author Peter Carl

    # DESCRIPTION:

    # A chart that shows rolling calculations of annualized return and annualized
    # standard deviation have proceeded through time.  Lines and dots are darker for
    # more recent time periods.

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


    # Code inspired by a chart on:
    # http://zoonek2.free.fr/UNIX/48_R/03.html

    x = checkData(R, method = "zoo")
    if(!is.null(dim(rf)))
        rf = checkData(rf, method = "zoo")
#     x = na.omit(x)
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)
    maxrows = 0

    add.labels = add.labels[1]

    if(first) { # start the calculations with the start date of the first column
        length.column.one = length(x[,1])
    # find the row number of the last NA in the first column
        start.row = 1
        start.index = 0
        while(is.na(x[start.row,1])){
            start.row = start.row + 1
        }
        x = x[start.row:length.column.one,,drop=FALSE]
    }

    # @todo: strip out basic elements to a scatter plot wrapper
    # Set color for key elements, easy to darken for the printer
    if(darken)
        elementcolor = "darkgray" #better for the printer
    else
        elementcolor = "lightgray" #better for the screen

    if(length(colorset) < columns)
        colorset = rep(colorset, length.out = columns)

    if(length(symbolset) < columns)
        symbolset = rep(symbolset, length.out = columns)

    plot.new()
###
    for(column in 1:columns) {
        # Assume we're passed in a series of monthly returns.  First, we'll
        # annualized returns and risk
        y = x[,column,drop=FALSE]
        y = na.omit(y)
#        y=x[,column,drop=FALSE]
        returns.column = rollapply(y[(nrow(y)\%\%stepsize+1):nrow(y),1,drop=FALSE], width = width, FUN = Return.annualized, na.pad = F, align = "right",by=stepsize)
    
        risk.column = rollapply(y[(nrow(y)\%\%stepsize+1):nrow(y),1,drop=FALSE], width = width, FUN = StdDev.annualized, na.pad = F, align = "right",by=stepsize)

        maxrows = max(maxrows, length(returns.column))

        if(column == 1){
            returns = returns.column
            risk = risk.column
        }
        else {
            returns = merge(returns,returns.column)
            risk = merge(risk,risk.column)
        }
    }

    # Set the charts to show the origin
    if(is.null(xlim[1]))
        xlim = c(0, max(risk, na.rm=TRUE) + 0.02)
    if(is.null(ylim[1]))
        ylim = c(min(c(0,returns), na.rm=TRUE), max(returns, na.rm=TRUE) + 0.02)
    plot.window(xlim=xlim, ylim=ylim)

    for(column in columns:1) {

        # colortrail = gray(0:(length(returns))/(length(returns)))
        colors = colorRamp(c(colorset[column],"white"))
        colortrail = rgb(colors((0:maxrows)/maxrows),max=255)
        n.rows = length(returns[,column])
        m.rows = length(na.omit(returns[,column]))
        # Draw the principal scatterplot
#         plot(returns ~ risk,
#             xlab='', ylab='',
#             las = 1, xlim=xlim, ylim=ylim, cex.axis = .8, col = colortrail[length(returns):1], pch = symbolset[columns:1], axes= FALSE, add=TRUE,...)

        for(i in 1:length(returns[,column])){
#             points(risk[i],returns[i], pch=symbolset[column], col = colortrail[length(returns)-i+1])
            points(risk[i,column],returns[i,column], pch=symbolset[column], col = colortrail[maxrows-i+1])
        }
        # Attach the points with lines
        for(i in 2:length(returns[,column])){
            segments(risk[i,column], returns[i,column], risk[i-1,column], returns[i-1,column],col = colortrail[maxrows-i+1], lty = lty, lwd = lwd)
        }

        # Label the data points
        if(!is.null(add.labels))
            if(add.labels == "lastonly"){
                labels = rep("",n.rows)
                labels[n.rows] = as.character(time(returns[n.rows,column]))
            }
            else if(add.labels == "firstandlast"){
                labels = rep("",n.rows)
                labels[n.rows] = as.character(time(returns[n.rows,column]))
                labels[(n.rows-m.rows+1)] = as.character(time(returns[(n.rows-m.rows+1),column]))
            }
            else if(add.labels == "all") {
                labels = time(returns)
            }
            else
                labels = NULL
            text(x = risk[,column,drop=FALSE],y = returns[,column,drop=FALSE], labels = labels, adj = -0.2, cex = cex.text, col = colortrail[maxrows:1])
    }
###

    if(ylim[1] != 0){
        abline(h = 0, col = elementcolor)
    }
    axis(1, cex.axis = cex.axis, col = elementcolor)
    axis(2, cex.axis = cex.axis, col = elementcolor)
    title(ylab = ylab, cex.lab = cex.lab)
    title(xlab = xlab, cex.lab = cex.lab)

    # Add Sharpe ratio lines
    # @todo: Drawing Sharpe ratio lines currently throws warnings; change test statement
    if(!is.na(add.sharpe[1])) {
        for(line in add.sharpe) {
        abline(a=(rf*12),b=add.sharpe[line],col="gray",lty=2)
        }
    }

    title(main = main, cex.main = cex.main) # @todo: put window into title by default

    if(!is.null(legend.loc)){
        # There's no good place to put this automatically, except under the graph.
        # That requires a different solution, but here's the quick fix
        legend(legend.loc, inset = 0.02, text.col = colorset, col = colorset, cex = 0.8, border.col = elementcolor, pch = symbolset, bg = "white", legend = columnnames)
    }

    #title(sub='From Inception', line=1)
    box(col = elementcolor)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
