\name{BetaCoVariance}
\alias{BetaCoVariance}
\alias{BetaCoV}
\alias{SystematicVariance}
\alias{SystematicBeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ systematic beta of an asset to an initial portfolio }
\description{
Beta covariance is the beta of an asset to the variance and covariance of an initial portfolio.  Used to determine diversification potential. also called "systematic beta" by several papers.

This function uses a symbolic representation to achieve the same result as the linear intercept model used by \code{\link{CAPM.beta}}
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
Systematic Beta \eqn{\beta_{a,b}}{BCoV(a,b)}

\eqn{\beta_{a,b}=\frac{CoV_{a,b}}{\sigma_{a}}=\frac{\sum((R_{a}-\bar{R_{a}})(R_{b}-\bar{R_{b}}))}{\sum(R_{a}-\bar{R_{a}})^{2}}}{BetaCoV=cov(a,b)/var(b)}

Ruppert(2004) reports that this equation will give the estimated slope of the linear regression of \eqn{R_{a}}{Ra}on \eqn{R_{b}}{Rb} and that this slope can be used to determine the risk premium or excess expected return (see Ruppert Eq. 7.9 and 7.10, p. 230-231)
}
\value{
systematic beta of the asset to the benchmark
}
\references{
Favre, L. and Renaldo, A.. How to Price Hedge Funds: From Two- to Four-Moment CAPM. October 2003. UBS and Edhec Business School. Equation [5] p 10
Ruppert, David. \emph{Statistics and Finance, an Introduction}. Springer. 2004.
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{CAPM.beta}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
