\name{CenteredMoments}
\alias{centeredcomoment}
\alias{centeredmoment}
\alias{Return.centered}
\alias{Ipower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate centered moments of a joint distribution }
\description{
the $n$-th centered moment is calculated as 
\begin{equation}
 \mu^{(n)}(R) = E\lbrack(R-E(R))^n\rbrack
\end{equation}

These functions are used internally by PerformanceAnalytics to calculate centered moments for a multivariate distribution as well as the standardized moments of a portfolio distribution.  They are exposed here for users who wish to use them directly, and we'll get more documentation written when we can.
}
\usage{
centeredcomoment (Ra, Rb, p1, p2, normalize = FALSE)
centeredmoment (R, power)
Return.centered (R, ...)
Ipower (power, h)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ a vector, matrix, data frame, timeSeries or zoo object of index, benchmark, portfolio, or secondary asset returns to compare against }
  \item{power}{ power or moment to calculate }
  \item{p1}{ first power of the comoment }
  \item{p2}{ second power of the comoment }
  \item{h}{ ~~Describe \code{h} here~~ }
  \item{normalize}{ whether to standardize the calculation to agree with common usage, or leave the default mathematical meaning }
  \item{\dots}{ any other passthru parameters }
}
\details{
These functions were first utilized in Boudt, Peterson, and Croux (2008), and have been subsequently used in our other research.

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
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
