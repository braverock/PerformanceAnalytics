\name{VaR.Beyond}
\alias{VaR.Beyond}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate BVaR or loss Beyond traditional mean-VaR }
\description{
Beyond VaR purports to estimate average loss beyond VaR.  Please note that your milage will vary; expect that values obtained from the normal distribution differs radically from the real situation.

BeyondVaR is described in theoretical detail in the paper:
Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R. "Beyond the VaR Horizon" 2001, Quants Review No. 37
}
\usage{
VaR.Beyond(R, p=.95, modified=FALSE, periods = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{modified}{ TRUE/FALSE default FALSE, use Cornish Fisher Expansion to take higher moments into account }
  \item{periods}{ number of periods to calculate predicted loss beyond the VaR for, default 1 }
}
\details{
We believe that the extension of Cornish-Fisher VaR as an option in the Beyond VaR calculation is unique to this library.
}
\value{
estimated Value of single period loss Beyond traditional mean-VaR
}
\references{
Gaussel, N., Legras, J., Longin, F., and Rabemananjara, R. "Beyond the VaR Horizon" 2001, Quants Review No. 37
}
\author{
Peter Carl \cr
Brian G. Peterson
}
\seealso{
    \code{\link{VaR.CornishFisher}}
    \code{\link{VaR.traditional}}
    \code{\link[fPortfolio]{CVaR}}
}
\examples{
    data(edhec)

    # first do normal Beyond VaR calc
    VaR.Beyond(edhec)

    # now use modified Cornish-Fisher VaR calc to take non-normal distribution into account
    VaR.Beyond(edhec,modified=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }