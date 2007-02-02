\name{rollingStat}
\alias{rollingStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rollingStat(R, period = 3, trim = TRUE, FUN, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{period}{ ~~Describe \code{period} here~~ }
  \item{trim}{ ~~Describe \code{trim} here~~ }
  \item{FUN}{ ~~Describe \code{FUN} here~~ }
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
function (R, period = 3, trim = TRUE, FUN, ...)
{ # @author Peter Carl

    # DESCRIPTION
    # Function to apply any time series function over a rolling period,
    # say the last three months.  Pass in the function name as FUN,
    # e.g., FUN = "mean".  Useful for passing in several periods and functions
    # for comparing different series.

    # Inputs:
    # period  the number of periods over which to calculate the statistic.
    #         Setting the period to 0 calculates the statistic "from inception" or
    #         using all of the data passed in.
    # FUN     function to apply over e.g., FUN = "mean" or FUN = "Return.cumulatives"

    # Output:

    # FUNCTION

    x = checkDataVector(R)

    if (length(x) < period){
    # @todo: instead of stopping, should this warn and return NAs for length(x)?
        warning("Data set is not long enough. Reduce the evaluation period or provide a longer time period of data. Also, check the data for NA's.")
#         answer=NA
    }
#     if (!is.na(answer)){
#     }
#     else {
        if (period == 0) {
            y = as.matrix(x[1:length(x)])
        }
        else {
            y = as.matrix(x[(length(x)- period + 1):length(x)])
        }
        answer=apply(y, MARGIN = 2, FUN = FUN, ...)
#        print("here")
#     }
#     else {
#         answer = NA
#     }
    return(answer)
    # Example includes how to pass in arguements to the function - must be in order:
    # > rollingStat(gg.ts@Data[,1],period=12,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 1.476426
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=.03/12)
    # [1] 6.804358
    # > rollingStat(gg.ts@Data[,1],period=3,FUN="SharpeRatio.annualized",rf=0)
    # [1] 8.253612

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
