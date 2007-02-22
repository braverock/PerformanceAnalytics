\name{VaR.Marginal}
\alias{VaR.Marginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the Marginal VaR of each element of a portfolio }
\description{
}
\usage{
VaR.Marginal(R, p = 0.99, modified = TRUE, weightingvector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns or the components }
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
}
\author{ Brian G. Peterson }
\note{
}
\seealso{
    \code{\link{VaR.CornishFisher}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }