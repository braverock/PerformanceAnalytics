\name{mean.utils}
\alias{mean.utils}
\alias{mean.geometric}
\alias{mean.ucl}
\alias{mean.lcl}
\alias{mean.stderr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate attributes relative to the mean of the return series given }
\description{
The Treynor ratio is similar to the Sharpe Ratio, except it uses beta as the volatility measure (to divide the investment's excess return over the beta). }
\usage{
mean.geometric(R)
mean.stderr(R)
mean.ucl(R, ci)
mean.lcl(R, ci)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{ci}{ the confidence interval to use }
}
\value{
value for the function called
}
\references{
}
\author{ Peter Carl }
\seealso{
   \code{\link{sd}}
}
\examples{
data(edhec)
mean.geometric(edhec[,"Funds.of.Funds"])
mean.stderr(edhec[,"Funds.of.Funds"])
mean.UCL(edhec[,"Funds.of.Funds"])
mean.LCL(edhec[,"Funds.of.Funds"])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }