\name{rollingCorrelation}
\alias{rollingCorrelation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rollingCorrelation(x, y, n, trim = TRUE, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data for the x axis, can take matrix,vector, or timeseries }
  \item{y}{ data for the y axis, can take matrix,vector, or timeseries }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{trim}{ TRUE/FALSE, whether to keep alignment caused by NA's }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
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
function (x, y, n, trim = TRUE, na.rm = FALSE, ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for providing n-period trailing correlations for the
    # data provided.  Inspired by rollFun() written by Diethelm Wurtz.

    # Specifically, we're wrapping the basic function cor(), which has:
    # function (x, y = NULL, use = "all.obs", method = c("pearson",
    #     "kendall", "spearman"))

    # Inputs:
    # Assumes an input of regular period returns.
    #    x: a numeric vector, matrix, data frame, or timeSeries.
    #       The resulting data frame will contain columns of cor(x,y)
    #       for each period.
    #
    #    y: 'NULL' (default) or a vector, matrix, data frame or timeSeries
    #       with compatible dimensions to 'x'.
    #
    #    Currently assumes that BOTH x and y are provided
    #    @todo: Make it so we can _either_ give a matrix or data frame
    #    for 'x' _or_ give both 'x' and 'y' (as in cov())
    #
    # Assumes that x and y are sequenced exactly the same and are regular.

    # Outputs:
    # A data.table of n-period trailing correlations for each column in y.

    # FUNCTION:

    # target.vec is the vector of data we want correlations for; we'll get it
    # from x
    target.vec = checkDataVector(x)
    xrows = nrow(x)
    rownames = rownames(x)

    # data.matrix is a vector or matrix of data we want correlations against;
    # we'll take it from y
    data.matrix = checkDataMatrix(y)
    columns=ncol(y)
    columnnames = colnames(y)

    if (!trim) {
        result.df = as.data.frame(matrix(data = NA, nrow = (n-1), ncol = columns, byrow = FALSE, dimnames = NULL))
        colnames(result.df) = columnnames
    }
    #  For each period (or row):
    for(row in n:xrows) {
        values = vector('numeric', 0)
        # Get the subset of returns on which to calulate correlation
        trailing.data = data.matrix[(row-n+1):row,]
        trailing.vec = target.vec[(row-n+1):row]

        # Calculate correlation
        values = cor(trailing.vec,trailing.data,...)

        if(row == n && trim) {
            result.df = data.frame(Value = values, row.names = rownames[n])
        }

        else {
            nextrow = data.frame(Value = values, row.names = rownames[row])
            colnames(nextrow) = columnnames
            result.df = rbind(result.df, nextrow)
        }
    }

    if (!trim) {
        rownames(result.df) = rownames
    }
    else
        rownames(result.df) = rownames(n:xrows)

    colnames(result.df) = columnnames

    result.df

    # Example:
    # > head(rollingCorrelation(manager.ts@Data[,1],edhec.ts@Data,n=12))
    #            Convertible Arbitrage CTA Global Distressed Securities
    # 2003-11-28             0.2591101  0.2762218             0.7516556
    # 2003-12-31             0.2162078  0.2477113             0.7452179
    # 2004-01-30             0.3918575  0.3489062             0.7562063
    # 2004-02-27             0.5331404  0.3905645             0.7088004
    # 2004-03-31             0.5730389  0.3010877             0.5694478
    # 2004-04-30             0.5146946  0.3762283             0.4374524
    #            Emerging Markets Equity Market Neutral Event Driven
    # 2003-11-28        0.6678183             0.4133534    0.6872263
    # 2003-12-31        0.7739188             0.3758590    0.8494740
    # 2004-01-30        0.7805586             0.4148372    0.8338275
    # 2004-02-27        0.7353874             0.3444552    0.8181069
    # 2004-03-31        0.7072259             0.2740104    0.6270411
    # 2004-04-30        0.4430823             0.3918143    0.4966760
    #            Fixed Income Arbitrage Funds of Funds Global Macro Long/Short Equity
    # 2003-11-28              0.6681346      0.7413791    0.5698372         0.4429661
    # 2003-12-31              0.6583804      0.7705968    0.5647109         0.6949394
    # 2004-01-30              0.7116529      0.7427369    0.5972492         0.6849833
    # 2004-02-27              0.7580839      0.6831231    0.6555057         0.6300653
    # 2004-03-31              0.7057303      0.6172516    0.6253257         0.5233462
    # 2004-04-30              0.7583853      0.5305704    0.6740996         0.3518156
    #            Merger Arbitrage Relative Value Short Selling
    # 2003-11-28        0.6890493      0.6858798   -0.23238159
    # 2003-12-31        0.7397387      0.8062422   -0.41123820
    # 2004-01-30        0.7219990      0.7883262   -0.36321115
    # 2004-02-27        0.6862517      0.7605837   -0.28807086
    # 2004-03-31        0.5921739      0.6766063   -0.24154960
    # 2004-04-30        0.5073817      0.5257858   -0.04663322

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
