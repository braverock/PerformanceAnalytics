\name{StdDev.annualized}
\alias{StdDev.annualized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate an annualized Standard Deviation }
\description{
To annualize standard deviation, we multiply by the square root of the number of observations per year.
Note that any annualized number should be viewed with suspicion if the number of observations is small.

\deqn{\sqrt{scale}\cdot\sqrt{\sigma}}
}
\usage{
StdDev.annualized(Ra, scale = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{scale}{ number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4) }
}
\value{
annualized standard deviation
}
\references{}
\author{ Peter Carl}
\seealso{
    \code{\link{StdDev}}
}
\examples{
data(edhec)
head(edhec[,6,drop=FALSE])
StdDev.annualized(edhec[,6])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
