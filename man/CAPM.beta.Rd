\name{CAPM.beta}
\alias{CAPM.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CAPM.beta(x, y, rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
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
function (x, y, rf = 0, digits = 4)
{ # @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating a CAPM beta.

    # Inputs:
    # x: vector of returns for the asset being tested
    # y: vector of returns for the benchmark the asset is being gauged against
    # x and y are assumed to be matching periods and NO TEST IS MADE TO CHECK
    # rf: risk free rate in the same periodicity as the returns.  May be a vector
    #     of the same length as x and y.

    # Output:
    #

    # FUNCTION:

    assetReturns.vec = checkDataVector(x)
    indexReturns.vec = checkDataVector(y)

    if (length(assetReturns.vec) != length(indexReturns.vec))
        stop("Returns to be assessed have unequal time periods. Are there NA's in the data?")

    # Make these excess returns
    assetExcessRet.vec = assetReturns.vec - rf
    indexExcessRet.vec = indexReturns.vec - rf

    # regress
    model.lm = lm(assetExcessRet.vec ~ indexExcessRet.vec)

#    alpha = coef(model.lm)[[1]]
    beta = coef(model.lm)[[2]]
    beta
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
