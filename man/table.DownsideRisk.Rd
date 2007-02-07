\name{table.DownsideRisk}
\alias{table.DownsideRisk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table.DownsideRisk(R, ci = 0.95, firstcolumn = 1, scale = 12, rf = 0, MAR = 0.1/12, p = 0.99, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ ~~Describe \code{ci} here~~ }
  \item{firstcolumn}{ ~~Describe \code{firstcolumn} here~~ }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{MAR}{ Minimum Acceptable Return, in the same periodicity as your returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{digits}{ ~~Describe \code{digits} here~~ }
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
function (R, ci = 0.95, firstcolumn = 1, scale = 12, rf = 0, MAR = .1/12, p= 0.99, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts

    # Inputs:
    # R: a regular timeseries of returns (rather than prices)

    # Output:
    # A table of estimates of downside risk measures

    #FUNCTION:

    y = checkDataMatrix(R)

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    # for each column, do the following:
    for(column in firstcolumn:columns) {
    x = as.vector(y[,column])
    x.length = length(x)
    x = x[!is.na(x)]
    x.na = x.length - length(x)
    z = c(
            DownsideDeviation(x,MAR=mean(x)),
            sd(subset(x,x>0)),
            sd(subset(x,x<0)),
            DownsideDeviation(x,MAR=MAR),
            DownsideDeviation(x,MAR=rf),
            DownsideDeviation(x,MAR=0),
            maxDrawdown(x),
            VaR.CornishFisher(x, p=p, modified = FALSE),
            VaR.Beyond(x, p=p),
            VaR.CornishFisher(x, p=p)
            )
    znames = c(
            "Semi Deviation",
            "Gain Deviation",
            "Loss Deviation",
            paste("Downside Deviation (MAR=",MAR*scale*100,"\%)", sep=""),
            paste("Downside Deviation (rf=",rf*scale*100,"\%)", sep=""),
            paste("Downside Deviation (0\%)", sep=""),
            "Maximum Drawdown",
            paste("VaR (",p*100,"\%)",sep=""),
            "Beyond VaR",
            paste("Modified VaR (",p*100,"\%)",sep="")
            )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = round(resultingtable, digits)
    ans

    #  For example:
    #     > table.DownsideRisk(monthlyReturns.ts,rf=.04/12)
    #                                     Actual    S&P500TR
    #     Semi Deviation                0.020849116  0.02913679
    #     Gain Deviation                0.023009623  0.01975342
    #     Loss Deviation                0.007740678  0.01344530
    #     Downside Deviation (MAR=10\%)  0.019826422  0.02944389
    #     Downside Deviation (rf=4\%)    0.016275404  0.02713448
    #     Downside Deviation (0\%)       0.014248969  0.02642777
    #     Maximum Drawdown             -0.052021280 -0.04080700
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
