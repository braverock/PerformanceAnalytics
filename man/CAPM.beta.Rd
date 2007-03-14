\name{CAPM.beta}
\alias{CAPM.beta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate CAPM beta }
\description{
CAPM Beta is the beta of an asset to the variance and covariance of an initial portfolio.  Used to determine diversification potential. also called "systematic beta" by several papers.

This function uses a linear intercept model to achieve the same results as the symbolic model used by \code{\link{BetaCoVariance}}
}
\usage{
CAPM.beta(Ra, Rb, rf = 0, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{rf}{ risk free rate, in same period as your returns }
  \item{digits}{ number of digits to round results to }
}
\details{
 \deqn{\beta_{a,b}=\frac{CoV_{a,b}}{\sigma_{a}}=\frac{\sum((R_{a}-\bar{R_{a}})(R_{b}-\bar{R_{b}}))}{\sum(R_{a}-\bar{R_{a}})^{2}}}{cov(Ra,Rb)/var(Ra)}

Ruppert(2004) reports that this equation will give the estimated slope of the linear regression of \eqn{R_{a}}{Ra} on \eqn{R_{b}}{Rb} and that this slope can be used to determine the risk premium or excess expected return (see Eq. 7.9 and 7.10, p. 230-231)

}
\value{
systematic beta of an asset to the index
}
\references{
Sharpe, W.F. Capital Asset Prices: A theory of market equilibrium under conditions of risk. \emph{Journal of finance}, vol 19, 1964, 425-442. \cr
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004. \cr
}
\author{ Peter Carl }
\note{}
\seealso{
    \code{\link{BetaCoVariance}}
    \code{\link{CAPM.alpha}}
    \code{\link{CAPM.utils}}
}
\examples{
data(edhec)
edhec.length = dim(edhec)[1]
start = rownames(edhec[1,])
start
end = rownames(edhec[edhec.length,])
rf = download.RiskFree(start = start, end = end)
sp500 = download.SP500PriceReturns(start = "1996-12-31", end = end)

CAPM.beta(edhec[,"Funds.of.Funds"], sp500, rf = rf)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }