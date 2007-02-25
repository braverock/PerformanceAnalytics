\name{table.HigherMoments}
\alias{table.HigherMoments}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table.HigherMoments(x, y, scale = 12, rf = 0, digits = 4, method = "moment")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
  \item{method}{ ~~Describe \code{method} here~~ }
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
function (x, y, scale = 12, rf = 0, digits = 4, method = "moment")
{# @author Peter Carl

    # DESCRIPTION:
    # Higher Moments Summary: Statistics and Stylized Facts
    #
    # Used to determine diversification potential.
    # Also called "systematic kurtosis" or "systematic cokurtosis" by several papers.
    #
    # as defined in
    # Martellini L., Vaissie M., Ziemann V., October 2005,
    # Investing in Hedge Funds:
    #   Adding Value through Active Style Allocation Decisions
    # Edhec Risk and Asset Management Research Centre

    # Inputs:
    # x = return vector of initial portfolio
    # y = return vector of asset being considered for addition to portfolio

    # Assumes inputs are monthly returns, do not contain NA's, and are
    # lined up correctly.

    # Outputs:
    # A table of parameters from a linear regression of excess returns

    # FUNCTION:

    # Prepare the data
    # target.vec is the vector of data we want correlations for; we'll get it
    # from x
    assetReturns.vec = checkDataVector(x)

    # Make these excess returns
    assetExcessRet.vec = assetReturns.vec - rf

    # data.matrix is a vector or matrix of data we want correlations against;
    # we'll take it from y
    indexes.mat = checkDataMatrix(y)
    columns=ncol(indexes.mat)
    columnnames = colnames(indexes.mat)
    if (is.null(columnnames))
        stop("Column names are missing.  If you want to pass in a sub-set of a timeSeries, you need to use seriesData(monthlyReturns.ts[,2]) to preserve the column names.")

    # for each column in the matrix, do the following:
    for(column in 1:columns) {

    benchmarkReturns.vec = as.vector(indexes.mat[,column])
    benchmarkReturns.vec.length = length(benchmarkReturns.vec)
    benchmarkReturns.vec = benchmarkReturns.vec[!is.na(benchmarkReturns.vec)]
    benchmarkReturns.vec.na = benchmarkReturns.vec.length - length(benchmarkReturns.vec)

        # make these excess returns, too
        indexExcessRet.vec = benchmarkReturns.vec - rf

        # a few calculations

    z = c(CoSkewness(assetReturns.vec, benchmarkReturns.vec, na.rm=FALSE),
            CoKurtosis(assetReturns.vec, benchmarkReturns.vec, na.rm=FALSE),
            BetaCoVariance(assetReturns.vec, benchmarkReturns.vec, na.rm=FALSE),
            BetaCoSkewness(assetReturns.vec, benchmarkReturns.vec, na.rm=FALSE),
            BetaCoKurtosis(assetReturns.vec, benchmarkReturns.vec, na.rm=FALSE, method=method),
            )
    znames = c(
            "CoSkewness",
            "CoKurtosis",
            "Beta CoVariance",
            "Beta CoSkewness",
            "Beta CoKurtosis",
            )
        if(column == 1) {
            result.df = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            result.df = cbind(result.df, nextcolumn)
        }
    }
    colnames(result.df) = columnnames
    result.df = base::round(result.df, digits)
    result.df

    #  For example:


  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
