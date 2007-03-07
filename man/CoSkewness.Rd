\name{CoSkewness}
\alias{CoSkewness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate the co-moment for skewness of two assets }
\description{
CoSkewness is the product of the third higher moments of two assets.
}
\usage{
CoSkewness(Ri, Ra, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ri}{ return vector of initial portfolio }
  \item{Ra}{ return vector of asset being considered for addition to portfolio }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
}
\details{
    \eqn{CoS_{a,b}=\sum((R_{a}-\bar{R_{a}})(R_{i}-\bar{R_{i}})^{2})}{\code{CoS=sum((Ra-mean(Ra))(Ri-mean(Ri)^2))}}
}
\value{
value of coskewness of Ri and Ra
}
\references{
Martellini L., Vaissie M., Ziemann V. October 2005. Investing in Hedge Funds: Adding Value through Active Style Allocation Decisions. Edhec Risk and Asset Management Research Centre. Equation [5] p. 10

Martellini L. and Ziemann V. 2005. Marginal Impacts on Portfolio Distributions. Working Paper, Edhec Risk and Asset Management Research Centre
}
\author{ Brian G. Peterson }
\note{}
\seealso{
    \code{\link{BetaCoSkewness}}
    \code{\link[fBasics]{skewness}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
