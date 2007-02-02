\name{ActivePremium}
\alias{ActivePremium}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ActivePremium(R, Ri, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ ~~Describe \code{R} here~~ }
  \item{Ri}{ ~~Describe \code{Ri} here~~ }
  \item{scale}{ ~~Describe \code{scale} here~~ }
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
function (R, Ri, scale = 12)
{ # @author Peter Carl

    # DESCRIPTION
    # ActivePremium = (Return.annualized(assetReturns.vec, scale = scale) -
    #                  Return.annualized(indexReturns.vec, scale = scale) )

    # Inputs:
    # Outputs:

    # FUNCTION
    assetReturns.vec = checkDataVector(R)
    indexReturns.vec = checkDataVector(Ri)

    ActivePremium = (Return.annualized(assetReturns.vec, scale = scale) - Return.annualized(indexReturns.vec, scale = scale))

    ActivePremium

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
