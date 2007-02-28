\name{Return.cumulative}
\alias{Return.cumulative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate a compounded (geometric) cumulative return }
\description{
This is a useful function for calculating cumulative return over a period of time, say a calendar year.  Can produce simple or geometric return.
}
\usage{
Return.cumulative(R, geometric = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
}
\value{
cumulative simple or geometric return
}
\references{ }
\author{ Peter Carl }
\seealso{
   \code{\link{Return.annualized}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }