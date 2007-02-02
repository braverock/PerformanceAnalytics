\name{BetaCoKurtosis}
\alias{BetaCoKurtosis}
\alias{BetaCoK}
\alias{SystematicKurtosis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BetaCoKurtosis(Ri, Ra, na.rm = FALSE, method = c("moment", "excess", "fisher"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ri}{ ~~Describe \code{Ri} here~~ }
  \item{Ra}{ ~~Describe \code{Ra} here~~ }
  \item{na.rm}{ ~~Describe \code{na.rm} here~~ }
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
function( Ri, Ra, na.rm=FALSE, method=c("moment", "excess", "fisher") )
{ # @author Brian G. Peterson

    # Description:
    # Beta CoKurtosis is the beta of an asset to the kurtosis
    # of an initial portfolio.  Used to determine diversification potential.
    # Also called "systematic kurtosis" or "systematic cokurtosis" by several papers.
    #
    # as defined in
    # Martellini L., Vaissie M., Ziemann V., October 2005,
    # Investing in Hedge Funds:
    #   Adding Value through Active Style Allocation Decisions
    # Edhec Risk and Asset Management Research Centre


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

    # Beta CoKurtosis of two assets
    ktB = CoKurtosis(Ra,Ri)/kurtosis(Ri, na.rm ,method ) #method = c("excess", "moment", "fisher")

    result = ktB

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
