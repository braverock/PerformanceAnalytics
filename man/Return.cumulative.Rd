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
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{geometric}{ generate geometric (TRUE) or simple (FALSE) returns, default TRUE }
}
\value{
cumulative simple or geometric return
}
\details{
product of all the individual period returns

\deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})-1=prod(1+R)-1}{prod(1+R)-1}

}
\references{
Bacon, Carl. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 6
}
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