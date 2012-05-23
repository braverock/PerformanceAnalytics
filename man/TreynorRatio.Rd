\name{TreynorRatio}
\alias{TreynorRatio}
\title{calculate Treynor Ratio of excess return over CAPM beta}
\usage{
  TreynorRatio(Ra, Rb, Rf = 0, scale = NA)
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}
}
\description{
  The Treynor ratio is similar to the Sharpe Ratio, except
  it uses beta as the volatility measure (to divide the
  investment's excess return over the beta).
}
\details{
  Equation:
  \deqn{\frac{\overline{(R_{a}-R_{f})}}{\beta_{a,b}}}{(mean(Ra-Rf))/(Beta(Ra,Rb))}
}
\examples{
data(managers)
round(TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12),4)
round(TreynorRatio(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE]),4)
round(TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf=.035/12),4)
round(TreynorRatio(managers[,1:6], managers[,8,drop=FALSE], Rf = managers[,10,drop=FALSE]),4)
round(TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf=.035/12),4)
round(TreynorRatio(managers[,1:6], managers[,8:7,drop=FALSE], Rf = managers[,10,drop=FALSE]),4)
}
\author{
  Peter Carl
}
\references{
  \url{http://en.wikipedia.org/wiki/Treynor_ratio}
}
\seealso{
  \code{\link{SharpeRatio}} \code{\link{SortinoRatio}}
  \code{\link{CAPM.beta}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

