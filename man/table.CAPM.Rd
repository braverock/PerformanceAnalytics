\name{table.CAPM}
\alias{table.CAPM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table.CAPM(x, y, scale = 12, rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
  \item{rf}{ ~~Describe \code{rf} here~~ }
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
function (x, y, scale = 12, rf = 0, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # Asset-Pricing Model Summary: Statistics and Stylized Facts
    #
    # Takes a set of returns and relates them to a market benchmark.
    # Provides a set of measures related to the excess return single index
    # model, or CAPM.

    # Inputs:
    # x: a vector of returns to test, e.g., the asset to be examined
    # y: a matrix, data.frame, or timeSeries of benchmarks to test the asset
    #     against.

    # This is irritating, but if you want to pass in a sub-set of a timeSeries,
    # you need to use
    # > seriesData(monthlyReturns.ts[,2])
    # rather than
    # > monthlyReturns.ts@Data[,2]
    # since the latter will strip the column names.

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
    indexes.matrix = checkDataMatrix(y)
    columns=ncol(indexes.matrix)
    columnnames = colnames(indexes.matrix)
    if (is.null(columnnames))
        stop("Column names are missing.  If you want to pass in a sub-set of a timeSeries, you need to use seriesData(monthlyReturns.ts[,2]) to preserve the column names.")

    # for each column in the matrix, do the following:
    for(column in 1:columns) {

    indexReturns.vec = as.vector(indexes.matrix[,column])
    indexReturns.vec.length = length(indexReturns.vec)
    indexReturns.vec = indexReturns.vec[!is.na(indexReturns.vec)]
    indexReturns.vec.na = indexReturns.vec.length - length(indexReturns.vec)

        # make these excess returns, too
        indexExcessRet.vec = indexReturns.vec - rf

        # a few calculations
        model.lm = lm(assetExcessRet.vec ~ indexExcessRet.vec)

        alpha = coef(model.lm)[[1]]
        beta = coef(model.lm)[[2]]
        htest = cor.test(assetReturns.vec, indexReturns.vec)
        ActivePremium = (Return.annualized(assetReturns.vec, scale = scale) - Return.annualized(indexReturns.vec, scale = scale))
        TrackingError = sqrt(sum(assetReturns.vec - indexReturns.vec)^2/(length(assetReturns.vec)-1)) * sqrt(scale)
        treynorRatio = (Return.annualized(assetReturns.vec, scale = scale) - rf*scale)/beta

    z = c(
            alpha,
            beta,
            summary(model.lm)$r.squared,
            ((1+alpha)^scale - 1),
            htest$estimate,
            htest$p.value,
            TrackingError,
            ActivePremium,
            ActivePremium/TrackingError,
            treynorRatio
            )
    znames = c(
            "Alpha",
            "Beta",
            "R-squared",
            "Annualized Alpha",
            "Correlation",
            "Correlation p-value",
            "Tracking Error",
            "Active Premium",
            "Information Ratio",
            "Treynor Ratio"
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
    result.df = round(result.df, digits)
    result.df

    #  For example:
    # > CAPMSummary(monthlyReturns.ts@Data[, 1],seriesData(monthlyReturns.ts[, 2]))
    #                     S&P500TR
    # Alpha                 0.0065
    # Beta                  0.2909
    # R-squared             0.0987
    # Annualized Alpha      0.0809
    # Correlation           0.3142
    # Correlation p-value   0.0547
    # Tracking Error        0.0149
    # Active Premium       -0.0083
    # Information Ratio    -0.5606
    # Treynor Ratio         0.3959

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
