\name{CoKurtosis}
\alias{CoKurtosis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate the co-moment for kurtosis of two assets }
\description{
  CoKurtosis is the product of the fourth higher moments of two assets.
}
\usage{
CoKurtosis( Ra, Ri, na.rm = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ return vector of asset being considered for addition to portfolio }
  \item{Ri}{ return vector of initial portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
}
\details{
    \deqn{CoK_{a,b}=\sum((R_{a}-\bar{R_{a}})(R_{i}-\bar{R_{i}})^{3})}{\code{CoS=sum((Ra-mean(Ra))(Ri-mean(Ri)^3))}}
}
\value{
value of cokurtosis of Ri and Ra
}
\references{
Martellini L., Vaissie M., Ziemann V. Investing in Hedge Funds: Adding Value through Active Style Allocation Decisions. October 2005. Edhec Risk and Asset Management Research Centre. Equation [5] p. 10

Martellini L. and Ziemann V. 2005. Marginal Impacts on Portfolio Distributions. Working Paper, Edhec Risk and Asset Management Research Centre
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{BetaCoKurtosis}}
    \code{\link[fBasics]{kurtosis}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
