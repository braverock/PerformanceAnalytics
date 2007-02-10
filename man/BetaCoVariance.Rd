\name{BetaCoVariance}
\alias{BetaCoVariance}
\alias{BetaCoV}
\alias{SystematicVariance}
\alias{SystematicBeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ systematic beta of an asset to an initial portfolio }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BetaCoVariance(Ri, Ra, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ri}{ return vector of initial portfolio }
  \item{Ra}{ return vector of asset being considered for addition to portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
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
\note{}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(Ri, Ra, na.rm=FALSE)
{ # @author Brian G. Peterson

    # Description:
    # Beta covariance is the beta of an asset to the variance and covariance
    # of an initial portfolio.  Used to determine diversification potential.
    # also called "systematic beta" by several papers
    #
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

    # CovarianceBeta of two assets
    covB = cov(Ra,Ri)/var(Ri)

    result = covB

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
