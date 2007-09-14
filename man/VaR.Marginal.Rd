\name{VaR.Marginal}
\alias{VaR.Marginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Marginal VaR of each element of a portfolio }
\description{
Marginal VaR is the difference between the VaR of the portfolio without the asset in question and the entire portfolio.  This function calculates Marginal VaR for all instruments in the portfolio.
}
\usage{
VaR.Marginal(R, p = 0.99, modified = TRUE, weightingvector=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns or the components }
  \item{p}{ confidence level for calculation, default p=.99 }
  \item{modified}{ TRUE/FALSE whether to use Cornish-Fisher VaR }
  \item{weightingvector}{ vector of weights for each element in the portfolio }
}
\details{
}
\value{
matrix of Marginal VaRs
}
\references{
Denton M. and Jayaraman, J.D. Incremental, Marginal, and Component VaR. Sunguard. 2004.
}
\author{ Brian G. Peterson }
\note{
Different papers call this different things.  In the Denton and Jayaraman paper referenced here, this calculation is called Incremental VaR.  We have chosen the more common usage of calling this difference in VaR's in portfolios without the instrument and with the instrument as the \dQuote{difference at the Margin}, thus the name Marginal VaR.  This is incredibly confusing, and hasn't been resolved in the literature at this time.
}
\seealso{
    \code{\link{VaR.CornishFisher}}
}
\examples{
    data(edhec)
    # construct a weighting vector for an equally weighted portfolio
    assets=dim(edhec)[2]
    eqweight=t(rep(1/assets, assets))
    VaR.Marginal(R=edhec,p=0.99,weightingvector=eqweight)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }