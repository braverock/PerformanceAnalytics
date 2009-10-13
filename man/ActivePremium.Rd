\name{ActivePremium}
\alias{ActivePremium}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Active Premium }
\description{
The return on an investment's annualized return minus the benchmark's annualized return.

Active Premium = Investment's annualized return - Benchmark's annualized return
}
\usage{
ActivePremium(Ra, Rb, scale = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ return vector of the portfolio }
  \item{Rb}{ return vector of the benchmark asset }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
ActivePremium (numeric)
}
\references{
Sharpe, W.F. The Sharpe Ratio,\emph{Journal of Portfolio Management},Fall 1994, 49-58.
}
\author{ Peter Carl }
\seealso{
    \code{\link{InformationRatio}}
    \code{\link{TrackingError}}
    \code{\link{Return.annualized}}
}
\examples{
    data(managers)
    ActivePremium(managers[, "HAM1", drop=FALSE], managers[, "SP500.TR", drop=FALSE])
    ActivePremium(managers[,1,drop=FALSE], managers[,8,drop=FALSE]) 
    ActivePremium(managers[,1:6], managers[,8,drop=FALSE]) 
    ActivePremium(managers[,1:6], managers[,8:7,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }