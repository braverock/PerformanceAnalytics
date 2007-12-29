\name{table.Autocorrelation}
\alias{table.Autocorrelation}
\title{ table of autocorrelation coefficients and Q statistics }
\description{
    This is a wrapper for calculating the first six autocorrelation
    coefficients and Q-statistic for significance against
    each column of the data provided.
}
\usage{
table.Autocorrelation(R, digits = 4)
}
\arguments{
  \item{R}{ a vector, matrix, or timeSeries of returns to test, e.g., the asset to be examined }
  \item{digits}{ number of digits to round results to }
}
\value{
    A data table of autocorrelation coefficients \eqn{\rho}{rho} and corresponding
    Q(6)-statistic for each column in R.
}
\references{  }
\author{ Peter Carl }
\examples{
## ~~ Need Example
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
