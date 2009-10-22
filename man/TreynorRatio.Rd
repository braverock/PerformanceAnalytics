\name{TreynorRatio}
\alias{TreynorRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate Treynor Ratio of excess return over CAPM beta }
\description{
The Treynor ratio is similar to the Sharpe Ratio, except it uses beta as the volatility measure (to divide the investment's excess return over the beta). }
\usage{
TreynorRatio(Ra, Rb, Rf = 0, scale = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{Rb}{ return vector of the benchmark asset }
  \item{Rf}{ risk free rate, in same period as your returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}

\details{
Equation:
\deqn{\frac{\overline{(R_{a}-R_{f})}}{\beta_{a,b}}}{(mean(Ra-Rf))/(Beta(Ra,Rb))}
}
\references{
\url{http://en.wikipedia.org/wiki/Treynor_ratio}
}
\author{ Peter Carl }
\seealso{
   \code{\link{SharpeRatio}}
   \code{\link{SortinoRatio}}
   \code{\link{CAPM.beta}}
}
\examples{
data(managers)
TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12) 
TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE]) 
TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf=.035/12) 
TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE])
TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf=.035/12) 
TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf = managers[,10,drop=FALSE])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }