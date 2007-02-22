\name{moment.fourth}
\alias{moment.fourth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate the fourth mathematical moment of the return function }
\description{
The fourth mathematical moment of the return function.
Favre and Renaldo use this as separate from kurtosis in developing a four-moment CAPM model.
}
\usage{
moment.fourth(R, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ return vector of the portfolio }
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
\references{
Favre, L. and Renaldo, A., October 2003. How to Price Hedge Funds: From Two- to Four-Moment CAPM. UBS and Edhec Business School.
}
\author{ Brian G. Peterson }
\seealso{
    \code{\link{moment.third}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(R,na.rm=FALSE)
{ # @author

Description:

    #
as defined in:
Favre, L. and Renaldo, A., October 2003
How to Price Hedge Funds: From Two- to Four-Moment CAPM
UBS and Edhec Business School

Setup

    R = as.vector(R)

    if(na.rm) {
        R <- R[!is.na(R)]
    }

FUNCTION:

    K = (mean((R-mean(R)^4)))^(1/4)

    result = K
Return Value:
    result
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
