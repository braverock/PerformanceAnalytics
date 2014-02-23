\name{Return.excess}
\alias{Return.excess}
\title{Calculates the returns of an asset in excess of the given risk free rate}
\usage{
Return.excess(R, Rf = 0)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rf}{risk free rate, in same period as your returns,
  or as a single digit average}
}
\description{
Calculates the returns of an asset in excess of the given
"risk free rate" for the period.
}
\details{
Ideally, your risk free rate will be for each period you
have returns observations, but a single average return for
the period will work too.

Mean of the period return minus the period risk free rate

\deqn{\overline{(R_{a}-R_{f})}}{mean(Ra-Rf=0)}

OR

mean of the period returns minus a single numeric risk free
rate

\deqn{\overline{R_{a}}-R_{f}}{mean(R)-rf}

Note that while we have, in keeping with common academic
usage, assumed that the second parameter will be a risk
free rate, you may also use any other timeseries as the
second argument.  A common alteration would be to use a
benchmark to produce excess returns over a specific
benchmark, as demonstrated in the examples below.
}
\examples{
data(managers)
head(Return.excess(managers[,1,drop=FALSE], managers[,10,drop=FALSE]))
head(Return.excess(managers[,1,drop=FALSE], .04/12))
head(Return.excess(managers[,1:6], managers[,10,drop=FALSE]))
head(Return.excess(managers[,1,drop=FALSE], managers[,8,drop=FALSE]))
}
\author{
Peter Carl
}
\references{
Bacon, Carl. \emph{Practical Portfolio Performance
Measurement and Attribution}. Wiley. 2004. p. 47-52
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

