\name{BetaCoSkewness}
\alias{BetaCoSkewness}
\alias{BetaCoS}
\alias{SystematicSkewness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BetaCoSkewness(Ri, Ra, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ri}{ ~~Describe \code{Ri} here~~ }
  \item{Ra}{ ~~Describe \code{Ra} here~~ }
  \item{na.rm}{ ~~Describe \code{na.rm} here~~ }
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
function(Ri, Ra, na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # Beta CoSkewness is the beta of an asset to the skewness
    # of an initial portfolio.  Used to determine diversification potential.
    # also called "systematic skewness" or "systematic co-skewness"
    # by several papers.
    # as defined in
    # Favre, L. and Renaldo, A., October 2003
    # How to Price Hedge Funds: From Two- to Four-Moment CAPM
    # UBS and Edhec Business School
    # Equation [5] p. 10

    # Ri = return vector of initial portfolio
    # Ra = return vector of asset being considered for addition to portfolio

    # Setup

    Ri = as.vector(Ri)
    Ra = as.vector(Ra)

    if(na.rm) {
        Ri <- Ri[!is.na(Ri)]
        Ra <- Ra[!is.na(Ra)]
    }

    # FUNCTION:

    # systematic skewness of two assets
    skB = CoSkewness(Ra,Ri)/(mean(Ri-mean(Ri)^3))

    result = skB

    # Return Value:
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
