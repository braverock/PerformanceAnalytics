\name{chart.BarVaR}
\alias{chart.BarVaR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.BarVaR(R, n = 0, risk.line = TRUE, method = "ModifiedVaR", reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab = "Value", xlab = "Date", date.format = "%m/%y", xlim = NA, ylim = NA, lwd = 1, colorset = (1:12), p = 0.99, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{risk.line}{ ~~Describe \code{risk.line} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{reference.grid}{ ~~Describe \code{reference.grid} here~~ }
  \item{xaxis}{ ~~Describe \code{xaxis} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{date.format}{ ~~Describe \code{date.format} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{ylim}{ ~~Describe \code{ylim} here~~ }
  \item{lwd}{ ~~Describe \code{lwd} here~~ }
  \item{colorset}{ ~~Describe \code{colorset} here~~ }
  \item{p}{ ~~Describe \code{p} here~~ }
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
function (R, n = 0, risk.line = TRUE, method = "ModifiedVaR", reference.grid = TRUE, xaxis = TRUE, main = "Title", ylab="Value", xlab="Date", date.format = "\%m/\%y", xlim = NA, ylim = NA, lwd = 1, colorset =(1:12), p=.99,...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper to create a chart of monthly returns in a bar chart.  This is
    # a difficult enough graph to read that it doesn't get much use.  Still,
    # it is useful for viewing a single set of data.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # n: periods specified for rolling-period calculations

    # Outputs:
    # A timeseries bar chart of the data series
    # VaR and Std Dev with n=0 are calculated from the start of the timeseries

    # FUNCTION:

    # Transform input data to a matrix
    x = checkDataMatrix(R)

    # Set up dimensions and labels
    columns = ncol(x)
    rows = nrow(x)
    columnnames = colnames(x)
    rownames = rownames(x)

    # Assume we're passed in a series of monthly returns.  First, we'll
    # calculate VaR or modified VaR
    #risk = as.matrix(rollingFunction(y, n = n, trim = FALSE, FUN = "VaR.CornishFisher"))

    if(risk.line){
        if(method == "StdDev"){
            # @todo: fix this nasty coersion
            #risk = as.matrix(rollingFunction(as.data.frame(y[,1]), n = n, trim = FALSE, FUN = "stdev"))
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "sd"))
            symmetric = TRUE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-month Std Dev",sep="")
            else
                legend.txt = "Std Dev"
        }
        if(method == "VaR"){
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "VaR", p = p, modified = FALSE))
            symmetric = TRUE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-Month VaR (1 Mo, ",p*100,"\%)",sep="")
            else
                legend.txt = paste("Traditional VaR (1 Mo, ",p*100,"\%)",sep="")
        }
        if(method == "ModifiedVaR"){
            risk = as.matrix(rollingFunction(as.data.frame(x[,1]), n = n, trim = FALSE, FUN = "VaR.CornishFisher", p = p))
            symmetric = FALSE
            if(n>0)
                legend.txt = paste("Rolling ",n,"-Month Modified VaR (1 Mo, ",p*100,"\%)",sep="")
            else
                legend.txt = paste("Modified VaR (1 Mo, ",p*100,"\%)",sep="")
        }
    }
    else {
        risk = 0
        legend.txt = ""
    }
    if(is.na(ylim[1])){
        ylim = range(c(x[!is.na(x[,1]),1],risk[!is.na(risk)],-risk[!is.na(risk)]))
    }

    chart.TimeSeries(x[,1], type = "h", col = colorset, legend.loc = NULL, ylim = ylim, reference.grid = reference.grid, xaxis = xaxis, main = main, ylab = ylab, xlab = xlab, lwd = lwd, lend="butt", ...)

    if(risk.line){
        if (symmetric)
            lines(1:rows, risk, col = colorset[1], lwd = 1, type = "l", lty="13")
        lines(1:rows, -risk, col = colorset[1], lwd = 1, type = "l", lty="13")
    }

    if(legend.txt != "")
        legend("bottomleft", inset = 0.02, text.col = colorset[1], col = colorset[1], cex = .8, border.col = "grey", lwd = 1, lty="13", bty = "n", legend = legend.txt)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
