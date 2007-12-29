\name{SmoothingIndex}
\alias{SmoothingIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Normalized Getmansky Smoothing Index  }
\description{
Proposed by Getmansky et al to provide a normalized measure of liquidity risk.  The index will produces  a number from zero to one.  A low number indicates low liquidity risk.  A number trending towards one indicates a higher liquidity risk.
}
\usage{
SmoothingIndex(Ra, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{\dots}{ any other passthru parameters }
}
\value{
a value ranging from 0 to 1 (not enforced in this function yet)
}
\details{


}
\references{
Chan, Nicholas, Mila Getmansky, Shane M. Haas, and Andrew W. Lo. 2005. Systemic Risk
  and Hedge Funds. NBER Working Paper Series (11200).
Getmansky, Mila, Andrew W. Lo, and Igor Makarov. 2004. An Econometric Model of Serial
  Correlation and Illiquidity in Hedge Fund Returns. Journal of Financial Economics (74):
  529–609.
}
\author{ Peter Carl }
\examples{
SmoothingIndex(managers[,8,drop=FALSE])
data(managers)
data(edhec)
indexes=merge(managers[,8:10],edhec)
indexes=na.omit(indexes)
sapply(indexes,FUN=SmoothingIndex)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }