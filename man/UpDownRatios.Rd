\name{UpDownRatios}
\alias{UpDownRatios}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
UpDownRatios(R, Rb, method = "capture", side = "up", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{side}{ ~~Describe \code{side} here~~ }
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
function(R, Rb, method = "capture", side = "up", ...)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a function designed to calculate several related metrics:
    #   Up (Down) Capture Ratio: this is a measure of an investment's compound
    #     return when the benchmark was up (down) divided by the benchmark's
    #     compound return when the benchmark was up (down). The greater (lower)
    #     the value, the better.
    #   Up (Down) Number Ratio: similarly, this is a measure of the number of
    #     periods that the investment was up (down) when the benchmark was up (down),
    #     divided by the number of periods that the Benchmark was up (down).
    #   Up (Down) Percentage Ratio: this is a measure of the number of periods
    #     that the investment outperformed the benchmark when the benchmark was
    #     up (down), divided by the number of periods that the benchmark was up (down).
    #     Unlike the prior two metrics, in both cases a higher value is better.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of regular period returns.
    # method: "capture", "number", or "percentage" to indicate which measure
    #     to return.
    # side: "Up" or "Down" statistic.

    # Outputs:
    # A data.table of n-period trailing calculations for each column
    # in x.

    # FUNCTION:
    assetReturns.vec = checkDataVector(R)
    benchmarkReturns.vec = checkDataVector(Rb)

    UpRi = subset(assetReturns.vec, assetReturns.vec > 0)
    DownRi = subset(assetReturns.vec, assetReturns.vec < 0)
    UpRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)
    DownRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)

    if(method == "capture" & side == "up") {
        UpRi = subset(assetReturns.vec, benchmarkReturns.vec > 0)
        UpRb = subset(benchmarkReturns.vec, benchmarkReturns.vec > 0)
        cumRi = prod(1+UpRi) - 1
        cumRb = prod(1+UpRb) - 1
        result = cumRi/cumRb
    }
    if(method == "capture" & side == "down") {
        DnRi = subset(assetReturns.vec, benchmarkReturns.vec < 0)
        DnRb = subset(benchmarkReturns.vec, benchmarkReturns.vec < 0)
        cumRi = prod(1+DnRi) - 1
        cumRb = prod(1+DnRb) - 1
        result = cumRi/cumRb
    }
    if(method == "number" & side == "up") {
        UpRi = length(subset(assetReturns.vec, benchmarkReturns.vec > 0))
        UpRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec > 0))
        result = UpRi/UpRb
    }
    if(method == "number" & side == "down") {
        DnRi = length(subset(assetReturns.vec, benchmarkReturns.vec < 0))
        DnRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec < 0))
        result = DnRi/DnRb
    }
    if(method == "percentage" & side == "up") {
        UpRi = length(subset(assetReturns.vec, (benchmarkReturns.vec > 0) && (benchmarkReturns.vec < assetReturns.vec)))
        UpRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec > 0))
        result = UpRi/UpRb
    }
    if(method == "percentage" & side == "down") {
        DnRi = length(subset(assetReturns.vec, (benchmarkReturns.vec < 0) && (benchmarkReturns.vec < assetReturns.vec)))
        DnRb = length(subset(benchmarkReturns.vec, benchmarkReturns.vec < 0))
        result = DnRi/DnRb
    }
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
