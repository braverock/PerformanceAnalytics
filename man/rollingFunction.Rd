\name{rollingFunction}
\alias{rollingFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rollingFunction(R, n, trim = TRUE, na.rm = TRUE, firstcolumn = 1, digits = 4, rf = 0, FUN = "mean", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{firstcolumn}{ first column to use from multicolumn input data R }
  \item{digits}{ number of digits to round results to }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{FUN}{ ~~Describe \code{FUN} here~~ }
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
function (R, n, trim = TRUE, na.rm = TRUE, firstcolumn = 1, digits = 4, rf = 0, FUN = "mean", ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for providing n-period trailing calculations for the
    # data and functions provided.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of regular period returns.
    # n: the number of periods over which a function is to be calculated. Use
    #     the value zero (0) to roll the statistic from inception
    # FUN: the function to apply to the data
    # trim:
    # na.rm:
    # firstcolumn:
    # digits: number of decimal places to round the display to.

    # Outputs:
    # A data.table of n-period trailing calculations for each column
    # in x.

    # Inspired by rollFun() written by Diethelm Wurtz.  We've extended the idea
    # to all the columns provided.

    # FUNCTION:

    data.mat = checkDataMatrix(R)
    columns=ncol(data.mat)
    # @todo: remove NA's before setting column names
    columnnames = colnames(data.mat)
    # @todo: handle empty column names
    if(is.null(columnnames))
        stop("Column names are empty.  If you are trying to pass in a timeSeries, use seriesData() rather than the explicit @Data slot.")
    rows = nrow(data.mat)
    rownames = rownames(data.mat)

    if(rows < n)
        stop("Data set is too short. Select a shorter evaluation period or provide a longer time period.  Also, check the data for NA's.")

    # for each column, do the following:
    for(column in firstcolumn:columns) {
        valueNames = vector('character', 0)
        values = vector('numeric', 0)
    x = checkDataVector(data.mat[,column])

        if(n == 0) {
            period = 0
            n = 12
        }
        else
            period = n

        for(row in n:rows) {
            subperiod = x[1:row]
            values = c(values,rollingStat(subperiod, period = period, FUN = FUN, ...))
            valueNames = c(valueNames,rownames[row])
        }
        if (!trim) {
            values = c(rep(NA, (n - 1)), values)
            valueNames = c(rownames[1:n-1],valueNames)
        }
        if(column == 1) {
            resultingtable = data.frame(Value = values, row.names = valueNames)
        }

        else {
            nextcolumn = data.frame(Value = values, row.names = valueNames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    result = base::round(resultingtable, digits)
    result

    # Examples:
    # > rollingFunction(gg.ts[,1],n=3,FUN="Return.annualized")
    #                     Manager
    # 2002-02-28           0.0306
    # 2002-03-31           0.0521
    # 2002-04-30           0.0387
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="Return.annualized")
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           0.0306
    # 2002-03-31           0.0521
    # 2002-04-30           0.0387
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized")
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           1.5302
    # 2002-03-31           4.3768
    # 2002-04-30           6.9640
    # ...
    # > rollingFunction(gg.ts[,1],n=3,trim=FALSE,FUN="SharpeRatio.annualized",rf=.03/12)
    #                     Manager
    # 2001-12-31               NA
    # 2002-01-31               NA
    # 2002-02-28           0.0298
    # 2002-03-31           1.8587
    # 2002-04-30           1.5598
    # ...
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
