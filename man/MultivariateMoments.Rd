\name{MultivariateMoments}
\alias{MultivariateMoments}
\alias{CoSkewnessMatrix}
\alias{CoKurtosisMatrix}
\alias{M3.MM}
\alias{M4.MM}
\alias{multivariate_mean}
\alias{StdDev.MM}
\alias{skewness.MM}
\alias{kurtosis.MM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ functions to compute multivariate moments of a joint distribution }
\description{
When worknig with multivariate series, the joint distribution of the returns is to be prefered over the simple univariate distribution of the resulting return vector of a portfolio.  These functions are used internally by PerformanceAnalytics to calculate centered moments for a multivariate distribution as well as the standardized moments of a portfolio distribution.  They are exposed here for users who wish to use them directly, and we'll get more documentation written when we can.
}
\usage{
M3.MM(R)
M4.MM(R)
StdDev.MM(w, sigma)
skewness.MM(w, sigma, M3)
kurtosis.MM(w, sigma, M4)
CoSkewnessMatrix(R, ...)
CoKurtosisMatrix(R, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{w}{ a vector of weights to be applied to each asset in the joint distribution }
%  \item{mu}{ mean or first centered moment of the distribution }
  \item{sigma}{ variance or second centered moment of the distribution }
  \item{M3}{ The third mathematical moment of the joint distribution, provided by  \code{\link{M3.MM}} }
  \item{M4}{ The fourth mathematical moment of the joint distribution, provided by  \code{\link{M4.MM}} }
  \item{\dots}{ any other passthru parameters }
}
\details{
  ~~ Additional Details will be added to documentation as soon as we have time to write them. ~~
}
\value{
}
\references{
Boudt, Kris, Brian G. Peterson, and Christophe Croux. 2008. Estimation and Decomposition
  of Downside Risk for Portfolios with Non-Normal Returns. Journal of Risk. Winter.

Martellini, Lionel, and Volker Ziemann. 2007. Improved Forecasts of Higher-Order Comoments and Implications for Portfolio Selection. EDHEC Risk and Asset Management Research Centre working paper.

Ranaldo, Angelo, and Laurent Favre Sr. 2005. How to Price Hedge Funds: From Two- to
  Four-Moment CAPM. SSRN eLibrary.

Scott, Robert C., and Philip A. Horvath. 1980. On the Direction of Preference for Moments of
  Higher Order than the Variance. Journal of Finance 35(4):915-919.

}
\author{ Kris Boudt, Brian Peterson }
\seealso{
\code{\link{skewness}}, \code{\link{kurtosis}}, \code{\link{MultivariateRisk}}, \code{\link{centeredmoment}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
